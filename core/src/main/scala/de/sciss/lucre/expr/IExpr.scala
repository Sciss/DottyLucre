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

package de.sciss.lucre.expr

import de.sciss.lucre.event.IChangePublisher
import de.sciss.lucre.stm.{Base, Disposable, Exec, Ref}

object IExpr {
  trait Var[T <: Exec[T], A] extends IExpr[T, A] with Ref[T, IExpr[T, A]]
}
trait IExpr[T <: Exec[T], +A] extends ExprLike[T, A] with IChangePublisher[T, A] with Disposable[T] {
  def value(implicit tx: T): A
}