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

package de.sciss.lucre.event

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.{Disposable, Elem, AnyTxn, TxSerializer, Txn}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput, Writable}

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

  /** Registers a live observer with this event. The method is called with the
    * observing function which receives the event's update messages, and the
    * method generates an opaque `Disposable` instance, which may be used to
    * remove the observer eventually (through the `dispose` method).
    */
  def react(fun: T => A => Unit)(implicit tx: T): Disposable[T]

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

  final def ---> (sink: Event[T, Any])(implicit tx: T): Unit = ()
  final def -/-> (sink: Event[T, Any])(implicit tx: T): Unit = ()

  final def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] = Observer.dummy[T]

  private[lucre] final def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A] = opNotSupported
}

object Event {
  implicit def serializer[T <: Txn[T]]: TxSerializer[T, Event[T, Any]] = anySer.asInstanceOf[Ser[T]]

  private val anySer = new Ser[AnyTxn]

  private[event] def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Event[T, Any] = {
    val slot  = in.readByte().toInt
    val node  = Elem.read[T](in, tx)
    node.event(slot)
  }

  private final class Ser[T <: Txn[T]] extends TxSerializer[T, Event[T, Any]] {
    def write(e: Event[T, Any], out: DataOutput): Unit = e.write(out)
    def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Event[T, Any] = Event.read(in, tx)
  }
}

/** `Event` is not sealed in order to allow you define traits inheriting from it, while the concrete
  * implementations should extend either of `Event.Constant` or `Event.Node` (which itself is sealed and
  * split into `Event.Invariant` and `Event.Mutating`.
  */
trait Event[T <: Txn[T], +A] extends EventLike[T, A] with Writable {
  // ---- abstract ----

  def node: Node[T]

  private[event] def slot: Int

  // ---- implemented ----

  final def ---> (sink: Event[T, Any])(implicit tx: T): Unit =
    node._targets.add(slot, sink)

  final def -/-> (sink: Event[T, Any])(implicit tx: T): Unit =
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

  final def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] = Observer(this, fun)
}