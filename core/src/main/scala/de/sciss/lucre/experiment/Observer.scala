/*
 *  Observer.scala
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

object Observer {
  def apply[T <: Txn[T], A](event: Event[T, A], fun: T => A => Unit)
                           (implicit tx: T): Observer[T, A] = {
    new Impl(event, tx, fun)
  }

//  def apply[T <: Exec[T], A](event: IEvent[T, A], fun: T => A => Unit)
//                            (implicit tx: T, targets: ITargets[T]): Observer[T, A] = {
//    new IImpl(event, tx, fun)
//  }
//
//  private final class IImpl[T <: Exec[T], A](event: IEvent[T, A], tx0: T, fun: T => A => Unit)
//                                            (implicit targets: ITargets[T])
//    extends Observer[T, A] {
//
//    override def toString = s"Observer<$event>"
//
//    targets.addEventReaction[A](event, this)(tx0)
//
//    def apply(update: A)(implicit tx: T): Unit = fun(tx)(update)
//
//    def dispose()(implicit tx: T): Unit = {
//      targets.removeEventReaction(event, this)
//    }
//  }
  
  private final class Impl[T <: Txn[T], A](event0: Event[T, A], tx0: T, fun: T => A => Unit)
    extends Observer[T, A] {

    override def toString = s"Observer<${event0.node.id}, ${event0.slot}>"

    private[this] val eventH = tx0.newHandle(event0: Event[T, Any])

    tx0.reactionMap.addEventReaction[A](event0, this)(tx0)

    def event(implicit tx: T): Event[T, Any] = eventH()

    def apply(update: A)(implicit tx: T): Unit = fun(tx)(update)

    def dispose()(implicit tx: T): Unit = {
      val event = this.event
      tx.reactionMap.removeEventReaction(event, this)
    }
  }
}
trait Observer[T <: Exec[T], -A] extends TDisposable[T] {
  def apply(update: A)(implicit tx: T): Unit
}