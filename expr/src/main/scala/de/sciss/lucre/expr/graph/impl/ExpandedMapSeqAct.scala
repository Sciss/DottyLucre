/*
 *  ExpandedMapSeqAct.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.expr.graph.{Act, It}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{IExpr, ITargets, Txn}

final class ExpandedMapSeqAct[T <: Txn[T], A](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                              fun: Act, tx0: T)
                                             (implicit protected val targets: ITargets[T], ctx: Context[T])
  extends IAction[T] with IActionImpl[T] {

  def executeAction()(implicit tx: T): Unit = {
    val inV = in.value
    if (inV.isEmpty) return

    val iterator  = inV.iterator
    while (iterator.hasNext) {
      val vn = iterator.next()
      it.setValue(vn)
      val (_, d) = ctx.nested(it) {
        val funEx = fun.expand[T] // ...which might be read here
        funEx.executeAction()
        ()
      }
      d.dispose()
    }
  }
}