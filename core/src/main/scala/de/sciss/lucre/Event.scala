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

package de.sciss.lucre

import de.sciss.equal.Implicits._
import de.sciss.lucre.Log.logEvent
import de.sciss.lucre.impl.CastTxnFormat
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.util.hashing.MurmurHash3

trait EventLike[T <: Txn[T], +A] extends Observable[T, A] {
  /** Connects the given selector to this event. That is, this event will
   * adds the selector to its propagation targets.
   */
  def ---> (sink: Event[T, Any])(implicit tx: T): Unit

  /** Disconnects the given selector from this event. That is, this event will
   * remove the selector from its propagation targets.
   */
  def -/-> (sink: Event[T, Any])(implicit tx: T): Unit

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
  private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A]
}

object Event {
  implicit def format[T <: Txn[T]]: TFormat[T, Event[T, Any]] = anyFmt.cast

  private val anyFmt = new Fmt[AnyTxn]

  private[lucre] def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Event[T, Any] = {
    val slot  = in.readByte().toInt
    val node  = Elem.read[T](in)
    node.event(slot)
  }

  private final class Fmt[T <: Txn[T]] extends CastTxnFormat[T, ({ type Repr[~ <: Txn[~]] = Event[~, Any] }) # Repr] {
    override def readT(in: DataInput)(implicit tx: T): Event[T, Any] =
      Event.read(in)
  }

  private type Children[T <: Txn[T]] = Vec[(Byte, Event[T, Any])]

  private def NoChildren[T <: Txn[T]]: Children[T] = Vector.empty

  object Targets {
    private implicit def childrenFormat[T <: Txn[T]]: TFormat[T, Children[T]] =
      anyChildrenFmt.asInstanceOf[ChildrenFmt[T]]

    private val anyChildrenFmt = new ChildrenFmt[AnyTxn]

    private final class ChildrenFmt[T <: Txn[T]] extends TFormat[T, Children[T]] {
      override def write(v: Children[T], out: DataOutput): Unit = {
        out./* PACKED */ writeInt(v.size)
        v.foreach { tup =>
          out.writeByte(tup._1)
          tup._2.write(out) // same as Selector.format.write(tup._2)
        }
      }

      override def readT(in: DataInput)(implicit tx: T): Children[T] = {
        val sz = in./* PACKED */ readInt()
        if (sz === 0) Vector.empty else Vector.fill(sz) {
          val slot  = in.readByte()
          val event = Event.read(in)
          (slot, event)
        }
      }
    }

    def apply[T <: Txn[T]]()(implicit tx: T): Targets[T] = {
      val id        = tx.newId()
      val children  = id.newVar /* newEventVar */[Children[T]](NoChildren)
      new Impl[T](0, id, children)
    }

    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Targets[T] = {
      (in.readByte(): @switch) match {
        case 0      => readIdentified(in)
        case cookie => sys.error(s"Unexpected cookie $cookie")
      }
    }

    /* private[lucre] */ def readIdentified[T <: Txn[T]](in: DataInput)(implicit tx: T): Targets[T] = {
      val id = tx.readId(in)
      val children = id.readVar /* readEventVar */[Children[T]](in)
      new Impl[T](0, id, children)
    }

    private final class Impl[T <: Txn[T]](cookie: Int, val id: Ident[T], childrenVar: Var[T, Children[T]])
      extends Targets[T] {

      def write(out: DataOutput): Unit = {
        out        .writeByte(cookie)
        id         .write(out)
        childrenVar.write(out)
      }

      def dispose()(implicit tx: T): Unit = {
        if (children.nonEmpty) throw new IllegalStateException("Disposing a event reactor which is still being observed")
        id         .dispose()
        childrenVar.dispose()
      }

      private[lucre] def children(implicit tx: T): Children[T] = childrenVar() // .getOrElse(NoChildren)

      override def toString = s"Targets$id"

      private[lucre] def add(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean = {
        logEvent(s"$this.add($slot, $sel)")
        val tup = (slot.toByte, sel)
        val seq = childrenVar() // .get // .getFresh
        logEvent(s"$this - old children = $seq")
        childrenVar() = seq :+ tup
        !seq.exists(_._1.toInt === slot)
      }

      private[lucre] def remove(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean = {
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

      def isEmpty (implicit tx: T): Boolean = children.isEmpty   // XXX TODO this is expensive
      def nonEmpty(implicit tx: T): Boolean = children.nonEmpty  // XXX TODO this is expensive

//      private[lucre] def _targets: Targets[T] = this
    }
  }

  /** An abstract trait unifying invariant and mutating targets. This object is responsible
   * for keeping track of the dependents of an event source which is defined as the outer
   * object, sharing the same `id` as its targets. As a `Reactor`, it has a method to
   * `propagate` a fired event.
   */
  sealed trait Targets[T <: Txn[T]] extends Mutable[/*Ident[T],*/ T] /* extends Reactor[T] */ {
    private[lucre] def children(implicit tx: T): Children[T]

    /** Adds a dependant to this node target.
     *
     * @param slot the slot for this node to be pushing to the dependant
     * @param sel  the target selector to which an event at slot `slot` will be pushed
     *
     * @return  `true` if this was the first dependant registered with the given slot, `false` otherwise
     */
    private[lucre] def add(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean

    def isEmpty (implicit tx: T): Boolean
    def nonEmpty(implicit tx: T): Boolean

    /** Removes a dependant from this node target.
     *
     * @param slot the slot for this node which is currently pushing to the dependant
     * @param sel  the target selector which was registered with the slot
     *
     * @return  `true` if this was the last dependant unregistered with the given slot, `false` otherwise
     */
    private[lucre] def remove(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean
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
  trait Node[T <: Txn[T]] extends Elem[T] with Mutable[T] {
    override def toString = s"Node$id"

    protected def targets: Targets[T]
    protected def writeData(out: DataOutput): Unit
    protected def disposeData()(implicit tx: T): Unit

    private[lucre] final def getTargets: Targets[T] = targets

    final def id: Ident[T] = targets.id

    final def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeId)
      targets.write(out)
      writeData(out)
    }

    final def dispose()(implicit tx: T): Unit = {
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

  private[lucre] def slot: Int

  // ---- implemented ----

  final def ---> (sink: Event[T, Any])(implicit tx: T): Unit =
    node.getTargets.add(slot, sink)

  final def -/-> (sink: Event[T, Any])(implicit tx: T): Unit =
    node.getTargets.remove(slot, sink)

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

  final def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] = Observer(this, fun)
}