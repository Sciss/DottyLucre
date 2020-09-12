/*
 *  IDummy.scala
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

import de.sciss.lucre.stm.{Exec, AnyExec, Disposable}
import de.sciss.model.Change

object IDummy {
  /** This method is cheap. */
  def apply[T <: Exec[T], A]: IEvent[T, A] = anyDummy.asInstanceOf[IEvent[T, A]]

  def applyChange[T <: Exec[T], A]: IChangeEvent[T, A] = anyChangeDummy.asInstanceOf[IChangeEvent[T, A]]

  private val anyDummy        = new Impl       [AnyExec]
  private val anyChangeDummy  = new ChangeImpl [AnyExec]

  private final class Impl[T <: Exec[T]] extends IEvent[T, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()
    def -/->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Any] = None

    def react(fun: T => Any => Unit)(implicit tx: T): Disposable[T] = Observer.dummy[T]
  }

  private final class ChangeImpl[T <: Exec[T]] extends IChangeEvent[T, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()
    def -/->(sink: IEvent[T, Any])(implicit tx: T): Unit = ()

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Any = ()

    def react(fun: T => Change[Any] => Unit)(implicit tx: T): Disposable[T] = Observer.dummy[T]
  }
}