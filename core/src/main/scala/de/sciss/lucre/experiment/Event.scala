/*
 *  Event.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.experiment

import de.sciss.equal.Implicits._
import de.sciss.lucre.experiment.Log.logEvent
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput, Writable}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.util.hashing.MurmurHash3

trait EventLike[T <: Txn[T], +A] extends Observable[T, A] {
  /** Connects the given selector to this event. That is, this event will
   * adds the selector to its propagation targets.
   */
  def ---> (sink: Event[T, Any]): Unit

  /** Disconnects the given selector from this event. That is, this event will
   * remove the selector from its propagation targets.
   */
  def -/-> (sink: Event[T, Any]): Unit

  /** Involves this event in the pull-phase of event delivery. The event should check
   * the source of the originally fired event, and if it identifies itself with that
   * source, cast the `update` to the appropriate type `A` and wrap it in an instance
   * of `Some`. If this event is not the source, it should invoke `pull` on any
   * appropriate event source feeding this event.
   *
   * @return  the `update` as seen through this event, or `None` if the event did not
   *          originate from this part of the dependency graph or was absorbed by
   *          a filtering function
   */
  private[experiment] def pullUpdate(pull: Pull[T]): Option[A]
}

object Dummy {
  /** This method is cheap. */
  def apply[T <: Txn[T], A]: Dummy[T, A] = anyDummy.asInstanceOf[Dummy[T, A]]

  private val anyDummy = new Impl[AnyTxn]

  private final class Impl[T <: Txn[T]] extends Dummy[T, Any] {
    override def toString = "event.Dummy"
  }

  private def opNotSupported = sys.error("Operation not supported")
}

trait Dummy[T <: Txn[T], +A] extends EventLike[T, A] {
  import Dummy._

  final def ---> (sink: Event[T, Any]): Unit = ()
  final def -/-> (sink: Event[T, Any]): Unit = ()

  final def react(fun: T => A => Unit)(implicit tx: T): TDisposable[T] = TDisposable.empty[T]

  private[lucre] final def pullUpdate(pull: Pull[T]): Option[A] = opNotSupported
}

object Event {
  implicit def serializer[T <: Txn[T]]: TSerializer[T, Event[T, Any]] = anySer.asInstanceOf[Ser[T]]

  private val anySer = new Ser[AnyTxn]

  private[experiment] def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Event[T, Any] = {
    val slot  = in.readByte().toInt
    val node  = Elem.read[T](in, tx)
    node.event(slot)
  }

  private final class Ser[T <: Txn[T]] extends TSerializer[T, Event[T, Any]] {
    def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Event[T, Any] = Event.read(in, tx)
    
    def write(e: Event[T, Any], out: DataOutput): Unit = e.write(out)
  }

  private type Children[T <: Txn[T]] = Vec[(Byte, Event[T, Any])]

  private def NoChildren[T <: Txn[T]]: Children[T] = Vector.empty

  object Targets {
    private implicit def childrenSerializer[T <: Txn[T]]: TSerializer[T, Children[T]] =
      anyChildrenSer.asInstanceOf[ChildrenSer[T]]

    private val anyChildrenSer = new ChildrenSer[AnyTxn]

    private final class ChildrenSer[T <: Txn[T]] extends TSerializer[T, Children[T]] {
      def write(v: Children[T], out: DataOutput): Unit = {
        out./* PACKED */ writeInt(v.size)
        v.foreach { tup =>
          out.writeByte(tup._1)
          tup._2.write(out) // same as Selector.serializer.write(tup._2)
        }
      }

      def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Children[T] = {
        val sz = in./* PACKED */ readInt()
        if (sz === 0) Vector.empty else Vector.fill(sz) {
          val slot  = in.readByte()
          val event = Event.read(in, tx)
          (slot, event)
        }
      }
    }

    def apply[T <: Txn[T]]()(implicit tx: T): Targets[T] = {
      val id        = tx.newId()
      val children  = id.newVar /* newEventVar */[Children[T]](NoChildren)
      new Impl[T](0, id, children)
    }

    def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Targets[T] = {
      (in.readByte(): @switch) match {
        case 0      => readIdentified(in, tx)
        case cookie => sys.error(s"Unexpected cookie $cookie")
      }
    }

    /* private[lucre] */ def readIdentified[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Targets[T] = {
      val id = tx.readId(in)
      val children = id.readVar /* readEventVar */[Children[T]](in)
      new Impl[T](0, id, children)
    }

    private final class Impl[T <: Txn[T]](cookie: Int, val id: Ident[T], childrenVar: /* event. */ Var[/*T,*/ Children[T]])
      extends Targets[T] {

      def write(out: DataOutput): Unit = {
        out        .writeByte(cookie)
        id         .write(out)
        childrenVar.write(out)
      }

      def dispose(): Unit = {
        if (children.nonEmpty) throw new IllegalStateException("Disposing a event reactor which is still being observed")
        id         .dispose()
        childrenVar.dispose()
      }

      private[experiment] def children: Children[T] = childrenVar() // .getOrElse(NoChildren)

      override def toString = s"Targets$id"

      private[experiment] def add(slot: Int, sel: Event[T, Any]): Boolean = {
        logEvent(s"$this.add($slot, $sel)")
        val tup = (slot.toByte, sel)
        val seq = childrenVar() // .get // .getFresh
        logEvent(s"$this - old children = $seq")
        childrenVar() = seq :+ tup
        !seq.exists(_._1.toInt === slot)
      }

      private[experiment] def remove(slot: Int, sel: Event[T, Any]): Boolean = {
        logEvent(s"$this.remove($slot, $sel)")
        val tup = (slot, sel)
        val xs = childrenVar() // .getOrElse(NoChildren)
        logEvent(s"$this - old children = $xs")
        val i = xs.indexOf(tup)
        if (i >= 0) {
          val xs1 = xs.patch(i, Vector.empty, 1) // XXX crappy way of removing a single element
          childrenVar() = xs1
          !xs1.exists(_._1.toInt === slot)
        } else {
          logEvent(s"$this - selector not found")
          false
        }
      }

      def isEmpty : Boolean = children.isEmpty   // XXX TODO this is expensive
      def nonEmpty: Boolean = children.nonEmpty  // XXX TODO this is expensive

      private[experiment] def _targets: Targets[T] = this
    }
  }

  /** An abstract trait unifying invariant and mutating targets. This object is responsible
   * for keeping track of the dependents of an event source which is defined as the outer
   * object, sharing the same `id` as its targets. As a `Reactor`, it has a method to
   * `propagate` a fired event.
   */
  sealed trait Targets[T <: Txn[T]] extends Mutable[/*Ident[T],*/ T] /* extends Reactor[T] */ {
    private[experiment] def children: Children[T]

    /** Adds a dependant to this node target.
     *
     * @param slot the slot for this node to be pushing to the dependant
     * @param sel  the target selector to which an event at slot `slot` will be pushed
     *
     * @return  `true` if this was the first dependant registered with the given slot, `false` otherwise
     */
    private[experiment] def add(slot: Int, sel: Event[T, Any]): Boolean

    def isEmpty : Boolean
    def nonEmpty: Boolean

    /** Removes a dependant from this node target.
     *
     * @param slot the slot for this node which is currently pushing to the dependant
     * @param sel  the target selector which was registered with the slot
     *
     * @return  `true` if this was the last dependant unregistered with the given slot, `false` otherwise
     */
    private[experiment] def remove(slot: Int, sel: Event[T, Any]): Boolean
  }

  /** XXX TODO -- this documentation is outdated.
   *
   * An `Event.Node` is most similar to EScala's `EventNode` class. It represents an observable
   * object and can also act as an observer itself. It adds the `Reactor` functionality in the
   * form of a proxy, forwarding to internally stored `Targets`. It also provides a final
   * implementation of the `Writable` and `Disposable` traits, asking sub classes to provide
   * methods `writeData` and `disposeData`. That way it is ensured that the sealed `Reactor` trait
   * is written first as the `Targets` stub, providing a means for partial deserialization during
   * the push phase of event propagation.
   *
   * This trait also implements `equals` and `hashCode` in terms of the `id` inherited from the
   * targets.
   */
  trait Node[T <: Txn[T]] extends Elem[T] with Mutable[/*Ident[T],*/ T] /* Obj[T] */ {
    override def toString = s"Node$id"

    protected def targets: Targets[T]
    protected def writeData(out: DataOutput): Unit
    protected def disposeData(): Unit

    private[experiment] final def _targets: Targets[T] = targets

    final def id: Ident[T] = targets.id

    final def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeId)
      targets.write(out)
      writeData(out)
    }

    final def dispose(): Unit = {
      disposeData() // call this first, as it may release events
      targets.dispose()
    }
  }
}

/** `Event` is not sealed in order to allow you define traits inheriting from it, while the concrete
 * implementations should extend either of `Event.Constant` or `Event.Node` (which itself is sealed and
 * split into `Event.Invariant` and `Event.Mutating`.
 */
trait Event[T <: Txn[T], +A] extends EventLike[T, A] with Writable {
  // ---- abstract ----

  def node: Event.Node[T]

  private[experiment] def slot: Int

  // ---- implemented ----

  final def ---> (sink: Event[T, Any]): Unit =
    node._targets.add(slot, sink)

  final def -/-> (sink: Event[T, Any]): Unit =
    node._targets.remove(slot, sink)

  final def write(out: DataOutput): Unit = {
    out.writeByte(slot)
    node.write(out)
  }

  override def hashCode: Int = {
    import MurmurHash3._
    val h0 = productSeed
    val h1 = mix(h0, slot)
    val h2 = mixLast(h1, node.hashCode)
    finalizeHash(h2, 2)
  }

  override def equals(that: Any): Boolean = that match {
    case thatEvent: Event[_, _] => slot === thatEvent.slot && node === thatEvent.asInstanceOf[Event[T, _]].node
    case _ => super.equals(that)
  }

  final def react(fun: T => A => Unit)(implicit tx: T): TDisposable[T] = Observer(this, fun)
}