/*
 *  ExprLike.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Base, Exec, Form}
import de.sciss.model.Change

/** This is the current compromise for unifying `Ex`/`IExpr` and `Expr`
  * in terms of their usability through `runWith` vs. `obj.attr`.
  */
trait ExprLike[T <: Exec[T], +A] extends Form[T] {
  def changed: Observable[T, Change[A]]

  def value(implicit tx: T): A
}
