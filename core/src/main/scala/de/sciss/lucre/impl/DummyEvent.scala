/*
 *  DummyEvent.scala
 *  (Lucre 4)
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
package impl

object DummyEvent {
  /** This method is cheap. */
  def apply[T <: Txn[T], A]: DummyEvent[T, A] = anyDummy.asInstanceOf[DummyEvent[T, A]]

  private val anyDummy = new Impl[AnyTxn]

  private final class Impl[T <: Txn[T]] extends DummyEvent[T, Any] {
    override def toString = "event.Dummy"
  }

  private def opNotSupported = sys.error("Operation not supported")
}

trait DummyEvent[T <: Txn[T], +A] extends EventLike[T, A] {
  import DummyEvent._

  final def ---> (sink: Event[T, Any])(implicit tx: T): Unit = ()
  final def -/-> (sink: Event[T, Any])(implicit tx: T): Unit = ()

  final def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] = Disposable.empty[T]

  private[lucre] final def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A] = opNotSupported
}