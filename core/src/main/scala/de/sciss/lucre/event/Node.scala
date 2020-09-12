/*
 *  Node.scala
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{AnyTxn, Elem, Ident, Mutable, TxSerializer, Txn, Var}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

object Targets {
  private implicit def childrenSerializer[T <: Txn[T]]: TxSerializer[T, Children[T]] =
    anyChildrenSer.asInstanceOf[ChildrenSer[T]]

  private val anyChildrenSer = new ChildrenSer[AnyTxn]

  private final class ChildrenSer[T <: Txn[T]] extends TxSerializer[T, Children[T]] {
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

  def apply[T <: Txn[T]](implicit tx: T): Targets[T] = {
    val id        = tx.newId()
    val children  = tx.newVar /* newEventVar */[Children[T]](id, NoChildren)
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
    val children = tx.readVar /* readEventVar */[Children[T]](id, in)
    new Impl[T](0, id, children)
  }

  private final class Impl[T <: Txn[T]](cookie: Int, val id: Ident[T], childrenVar: /* event. */ Var[T, Children[T]])
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

    private[event] def children(implicit tx: T): Children[T] = childrenVar() // .getOrElse(NoChildren)

    override def toString = s"Targets$id"

    private[event] def add(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean = {
      logEvent(s"$this.add($slot, $sel)")
      val tup = (slot.toByte, sel)
      val seq = childrenVar() // .get // .getFresh
      logEvent(s"$this - old children = $seq")
      childrenVar() = seq :+ tup
      !seq.exists(_._1.toInt === slot)
    }

    private[event] def remove(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean = {
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

    private[event] def _targets: Targets[T] = this
  }
}

/** An abstract trait unifying invariant and mutating targets. This object is responsible
  * for keeping track of the dependents of an event source which is defined as the outer
  * object, sharing the same `id` as its targets. As a `Reactor`, it has a method to
  * `propagate` a fired event.
  */
sealed trait Targets[T <: Txn[T]] extends stm.Mutable[/*Ident[T],*/ T] /* extends Reactor[T] */ {
  private[event] def children(implicit tx: T): Children[T]

  /** Adds a dependant to this node target.
    *
    * @param slot the slot for this node to be pushing to the dependant
    * @param sel  the target selector to which an event at slot `slot` will be pushed
    *
    * @return  `true` if this was the first dependant registered with the given slot, `false` otherwise
    */
  private[event] def add(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean

  def isEmpty (implicit tx: T): Boolean
  def nonEmpty(implicit tx: T): Boolean

  /** Removes a dependant from this node target.
    *
    * @param slot the slot for this node which is currently pushing to the dependant
    * @param sel  the target selector which was registered with the slot
    *
    * @return  `true` if this was the last dependant unregistered with the given slot, `false` otherwise
    */
  private[event] def remove(slot: Int, sel: Event[T, Any])(implicit tx: T): Boolean
}

/** XXX TODO -- this documentation is outdated.
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
  protected def disposeData()(implicit tx: T): Unit

  private[event] final def _targets: Targets[T] = targets

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