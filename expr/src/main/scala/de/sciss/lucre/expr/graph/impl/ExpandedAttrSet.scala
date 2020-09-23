/*
 *  ExpandedAttrSet.scala
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

import de.sciss.lucre.expr.CellView
import de.sciss.lucre.expr.graph.{Attr, Obj}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.{IExpr, Txn}

final class ExpandedAttrSet[T <: Txn[T], A](attrView: CellView.Var[T, Option[A]], value: IExpr[T, A], tx0: T)
  extends IActionImpl[T] {

  def executeAction()(implicit tx: T): Unit = {
    val v = value.value
    attrView.update(Some(v))
  }
}

final class ExpandedAttrSetIn[T <: Txn[T], A](in: IExpr[T, Obj], key: String, value: IExpr[T, A], tx0: T)
                                             (implicit bridge: Obj.Bridge[A])
  extends IActionImpl[T] {

  def executeAction()(implicit tx: T): Unit = {
    val v       = value.value
    val obj     = in.value
    val viewOpt = Attr.resolveNestedIn[T, A](obj.peer, key)
    viewOpt.foreach { attrView =>
      attrView.update(Some(v))
    }
  }
}
