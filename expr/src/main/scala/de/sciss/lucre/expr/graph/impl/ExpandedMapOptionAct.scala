/*
 *  ExpandedMapOptionAct.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.expr.graph.{Act, It}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{IExpr, ITargets, Txn}

final class ExpandedMapOptionAct[T <: Txn[T], A](in: IExpr[T, Option[A]], it: It.Expanded[T, A], fun: Act, tx0: T)
                                                (implicit protected val targets: ITargets[T], ctx: Context[T])
  extends IAction.Option[T] with IActionImpl[T] {

  def isDefined(implicit tx: T): Boolean = in.value.isDefined

  def executeAction()(implicit tx: T): Unit = executeIfDefined()

  def executeIfDefined()(implicit tx: T): Boolean = {
    val inOption = in.value
    inOption.exists { v =>
      it.setValue(v)
      val (_, d) = ctx.nested(it) {
        val actEx = fun.expand[T]
        actEx.executeAction()
      }

      d.dispose()
      true
    }
  }
}
