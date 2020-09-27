/*
 *  Debug.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.{IExpr, Txn}

object Debug {
  object PrintNow {
    private final class Expanded[T <: Txn[T]](text: IExpr[T, String]) extends IActionImpl[T] {
      def executeAction()(implicit tx: T): Unit = {
        val s = text.value
        print(s)
      }
    }
  }
  /** An action that prints a text expression "now" that is directly inside a transaction,
    * not waiting for the commit of the transaction. This is for debugging purposes, when
    * chasing exceptions which would otherwise swallow regular `PrintLn` actions.
    * Note that this action does not append a new-line, so one has to include a `\n` in
    * the string if needed.
    */
  final case class PrintNow(text: Ex[String]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Debug$$PrintNow"  // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new PrintNow.Expanded(text.expand[T])
  }
}
