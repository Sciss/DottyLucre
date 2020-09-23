/*
 *  StmObjAttrMapCellView.scala
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
import de.sciss.lucre.{Disposable, Txn, Obj => LObj}

// XXX TODO --- unfortunate that despite MapLike we have to distinguish
// because LObj.AttrMap must be put into a handle...

/** A `CellView[T, Option[LObj[T]]` built from an `LObj.attr` */
final class StmObjAttrMapCellView[T <: Txn[T]](attr0: LObj.AttrMap[T], key: String, tx0: T)
  extends CellView[T, Option[LObj[T]]] {

  private[this] val attrH = tx0.newHandle(attr0)

  private def attr(implicit tx: T) = attrH()

  def apply()(implicit tx: T): Option[LObj[T]] =
    attr.get(key)

  def react(fun: T => Option[LObj[T]] => Unit)(implicit tx: T): Disposable[T] = {
    attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case LObj.AttrAdded(`key`, obj) =>
          fun(tx)(Some(obj))

        case LObj.AttrRemoved(`key`, _) =>
          fun(tx)(None)

        case LObj.AttrReplaced(`key`, _, now) =>
          fun(tx)(Some(now))

        case _ => // ignore
      }
    }
  }
}
