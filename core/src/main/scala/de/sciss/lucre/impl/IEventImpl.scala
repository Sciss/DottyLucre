/*
 *  IEventImpl.scala
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
package impl

trait IEventImpl[T <: Exec[T], +A] extends IEvent[T, A] {
  protected def targets: ITargets[T]
  
  def --->(sink: IEvent[T, Any])(implicit tx: T): Unit =
    targets.add(this, sink)

  def -/->(sink: IEvent[T, Any])(implicit tx: T): Unit =
    targets.remove(this, sink)

  def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] =
    Observer(this, fun)(tx, targets)
}