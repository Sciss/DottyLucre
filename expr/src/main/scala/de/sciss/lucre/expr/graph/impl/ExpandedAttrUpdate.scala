/*
 *  ExpandedAttrUpdate.scala
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

import de.sciss.lucre.expr.CellView
import de.sciss.lucre.expr.graph.{Attr, Obj}
import de.sciss.lucre.{Disposable, IExpr, Txn}

final class ExpandedAttrUpdate[T <: Txn[T], A](source: IExpr[T, A], attrView: CellView.Var[T, Option[A]], tx0: T)
  extends Disposable[T] {

  private[this] val obs = source.changed.react { implicit tx => upd =>
    val v = Some(upd.now)
    attrView.update(v)
  } (tx0)

  def dispose()(implicit tx: T): Unit =
    obs.dispose()
}

final class ExpandedAttrUpdateIn[T <: Txn[T], A](in: IExpr[T, Obj], key: String, value: IExpr[T, A], tx0: T)
                                                (implicit bridge: Obj.Bridge[A])
  extends Disposable[T] {

  private[this] val obs = value.changed.react { implicit tx => upd =>
    val v       = Some(upd.now)
    val obj     = in.value
    val viewOpt = Attr.resolveNestedIn[T, A](obj.peer, key)
    viewOpt.foreach { attrView =>
      attrView.update(v)
    }
  } (tx0)

  def dispose()(implicit tx: T): Unit =
    obs.dispose()
}
