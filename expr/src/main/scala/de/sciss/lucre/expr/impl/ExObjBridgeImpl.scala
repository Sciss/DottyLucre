/*
 *  ExObjBridgeImpl.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.{CellView, Context}
import de.sciss.lucre.{Disposable, Txn, Expr => _Expr, Obj => LObj}
import de.sciss.serial.{DataOutput, TFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

abstract class AbstractExObjBridgeImpl[A, B <: A, _Ex[~ <: Txn[~]] <: _Expr[~, B]](tpe: _Expr.Type[B, _Ex])
  extends Obj.CanMake[A] with Obj.Bridge[A] {

  type Repr[T <: Txn[T]] = _Ex[T]

  protected def encode(in: A): B

  final def toObj[T <: Txn[T]](value: A)(implicit tx: T): _Ex[T] =
    tpe.newVar(tpe.newConst(encode(value)))

  final def reprTFormat[T <: Txn[T]]: TFormat[T, _Ex[T]] = tpe.format

  final def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[A]] =
    new AbstractCtxCellView[T, A](context.attr, key) {
      protected def tryParseValue(value: Any)(implicit tx: T): Option[A] =
        tpe.tryParse(value)

      protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[A] =
        if (obj.tpe === tpe) Some(obj.asInstanceOf[_Ex[T]].value) else None
    }

  final def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[A] = {
    val opt = obj.attr.get(key)
    opt match {
      case Some(v) if v.tpe.typeId === tpe.typeId =>
        val vt = v.asInstanceOf[_Ex[T]]
        Some(vt.value)

      case _ => None
    }
  }

  final def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[A] =
    if (obj.tpe.typeId === tpe.typeId) {
      val vt = obj.asInstanceOf[_Ex[T]]
      Some(vt.value)
    } else {
      None
    }

  override final def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(tpe.typeId)
  }
}

final class ExObjBridgeImpl[A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](tpe: _Expr.Type[A, _Ex])
  extends AbstractExObjBridgeImpl[A, A, _Ex](tpe) {

  def id: Int = _Expr.Type.ObjBridge.id

  protected def encode(in: A): A = in

  def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[A]] =
    CellView.attrUndoOpt[T, A, _Ex](map = obj.attr, key = key)(tx, tpe)
}

final class ExSeqObjBridgeImpl[A, _Ex[~ <: Txn[~]] <: _Expr[~, Vec[A]]](tpe: _Expr.Type[Vec[A], _Ex])
  extends AbstractExObjBridgeImpl[Seq[A], Vec[A], _Ex](tpe) {

  def id: Int = _Expr.Type.SeqObjBridge.id

  protected def encode(in: Seq[A]): Vec[A] = in.toIndexedSeq

  def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Seq[A]]] = {
    val peer = CellView.attrUndoOpt[T, Vec[A], _Ex](map = obj.attr, key = key)(tx, tpe)
    new CellView.Var[T, Option[Seq[A]]] {
      def update(v: Option[Seq[A]])(implicit tx: T): Unit =
        peer.update(v.map(_.toIndexedSeq))

      def apply()(implicit tx: T): Option[Seq[A]] = peer()

      def react(fun: T => Option[Seq[A]] => Unit)(implicit tx: T): Disposable[T] =
        peer.react(fun)
    }
  }
}