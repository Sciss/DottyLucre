/*
 *  PrintLn.scala
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
package graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.{IExpr, Txn}

object PrintLn {
  private final class Expanded[T <: Txn[T]](text: IExpr[T, String]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit = {
      val s = text.value
      tx.afterCommit {
        println(s)
      }
    }
  }
}
final case class PrintLn(text: Ex[String]) extends Act {
  type Repr[T <: Txn[T]] = IAction[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new PrintLn.Expanded(text.expand[T])
}
