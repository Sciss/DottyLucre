/*
 *  IEvent.scala
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

object IExpr {
  trait Var[T <: Exec[T], A] extends IExpr[T, A] with TVar[T, IExpr[T, A]]
}
trait IExpr[T <: Exec[T], +A] extends ExprLike[T, A] with IChangePublisher[T, A] with TDisposable[T] {
  def value(implicit tx: T): A
}