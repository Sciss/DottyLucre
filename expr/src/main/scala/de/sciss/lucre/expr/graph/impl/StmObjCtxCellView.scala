/*
 *  StmObjCtxCellView.scala
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

import de.sciss.lucre.expr.{CellView, Context, graph}
import de.sciss.lucre.{Disposable, ExprLike, Form, MapObjLike, Txn, Obj => LObj}

// XXX TODO --- unfortunate that despite MapObjLike we have to distinguish
// because LObj.AttrMap must be put into a handle...

abstract class AbstractCtxCellView[T <: Txn[T], A](attr: Context.Attr[T], key: String)
  extends CellView/*.Var*/[T, Option[A]] {

  // ---- abstract ----

  protected def tryParseValue(value: Any     )(implicit tx: T): Option[A]
  protected def tryParseObj  (obj  : LObj[T] )(implicit tx: T): Option[A]

  // ---- impl ----

//  def update(v: Option[A])(implicit tx: T): Unit =
//    v.foreach { value =>
//      attr.get(key).foreach {
//        case ex: graph.Var.Expanded[T, _] =>
//          def inner[A1](vr: graph.Var.Expanded[T, A1]): Unit =
//            vr.fromAny.fromAny(value).foreach { valueT =>
//              // XXX TODO --- support UndoManager
//              vr.update(new graph.Const.Expanded(valueT))
//            }
//
//          inner(ex)
//
//        case _ =>
//      }
//    }

  final def apply()(implicit tx: T): Option[A] =
    attr.get(key).flatMap(formValue)

  private def formValue(f: Form[T])(implicit tx: T): Option[A] = f match {
    case ex: ExprLike[T, _] => tryParseValue(ex.value)
    case obj: LObj[T]    => tryParseObj  (obj)
    case _                  => None
  }

  final def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
    val r1 = attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case MapObjLike.Added  (`key`, f) =>
          val opt = formValue(f)
          if (opt.isDefined) fun(tx)(opt)

        case MapObjLike.Removed(`key`, f) =>
          val opt = formValue(f)
          if (opt.isDefined) fun(tx)(None)

        case MapObjLike.Replaced(`key`, before, now) =>
          val optB = formValue(before)
          val optN = formValue(now  )
          if (optB != optN) fun(tx)(optN)

        case _ => // ignore
      }
    }
    attr.get(key) match {
      case Some(ex: ExprLike[T, _]) =>
        val r2 = ex.changed.react { implicit tx => upd =>
          val vOpt = tryParseValue(upd.now)
          fun(tx)(vOpt)
        }
        Disposable.seq(r1, r2)

      case _ => r1
    }
  }
}

/** A `CellView[T, Option[LObj[T]]` built from a `Context`. */
final class StmObjCtxCellView[T <: Txn[T]](attr: Context.Attr[T], key: String)
  extends AbstractCtxCellView[T, LObj[T]](attr, key) {

  protected def tryParseValue(value: Any)(implicit tx: T): Option[LObj[T]] = value match {
    case obj: graph.Obj => obj.peer
    case _              => None
  }

  protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[LObj[T]] = Some(obj)
}
