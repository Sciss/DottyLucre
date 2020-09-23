/*
 *  CellView.scala
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

import de.sciss.lucre.expr.impl.CellViewImpl.{FlatMapImpl, MapImpl, OptionOrElseImpl}
import de.sciss.lucre.expr.impl.{CellViewImpl => Impl}
import de.sciss.lucre.{Disposable, Expr, MapObj, Obj, Observable, Sink, Source, StringObj, Txn}
import de.sciss.serial.TFormat

object CellView {
  def empty[Tx, A]: CellView[Tx, Option[A]] = Empty.asInstanceOf[CellView[Tx, Option[A]]]

  def expr[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: Expr[~, A]](x: _Ex[T])(implicit tx: T,
                                                                      tpe: Expr.Type[A, _Ex]): CellView[T, A] =
    Impl.expr[T, A, _Ex](x)

  def exprMap[T <: Txn[T], K, A, _Ex[~ <: Txn[~]] <: Expr[~, A]](map: MapObj[T, K, _Ex], key: K)
                                                                (implicit tx: T, tpe: Expr.Type[A, _Ex],
                                                                 keyType: MapObj.Key[K]): CellView[T, Option[A]] =
    Impl.exprMap[T, K, A, _Ex](map, key)

  def attr[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](map: Obj.AttrMap[T], key: String)
                                                        (implicit tx: T, tpe: Expr.Type[A, E]): CellView.Var[T, Option[A]] =
    Impl.attr[T, A, E](map, key)

  /** Additionally uses undo manager when present. */
  def attrUndoOpt[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](map: Obj.AttrMap[T], key: String)
                                                               (implicit tx: T, tpe: Expr.Type[A, E]): CellView.Var[T, Option[A]] =
    Impl.attrUndoOpt[T, A, E](map, key)

  def name[T <: Txn[T]](obj: Obj[T])(implicit tx: T): CellView[T, String] = {
    implicit val stringEx: Expr.Type[String, StringObj] = StringObj
    attr[T, String, StringObj](obj.attr, Obj.attrName).map(_.getOrElse("<unnamed>"))
  }

  def exprLike[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: Expr[~, A]](x: _Ex[T])
                                                              (implicit tx: T,
                                                               format: TFormat[T, _Ex[T]]): CellView[T, A] =
    Impl.exprLike[T, A, _Ex](x)

  def const[T <: Txn[T], A](value: A): CellView[T, A] =
    Impl.const[T, A](value)

  object Var {
    def unapply[Tx, A](view: CellView[Tx, A]): Option[Var[Tx, A]] = view match {
      case vr: Var[Tx, A] => Some(vr)
      case _              => None
    }

    def empty[Tx, A]: Var[Tx, Option[A]] = Empty.asInstanceOf[Var[Tx, Option[A]]]

    private object Empty extends Var[Any, Option[Nothing]] {
      def react(fun: Any => Option[Nothing] => Unit)(implicit tx: Any): Disposable[Any] =
        Disposable.empty

      def apply()(implicit tx: Any): Option[Nothing] = None

      def update(v: Option[Nothing])(implicit tx: Any): Unit = ()
    }
  }
  trait Var[Tx, A] extends CellView[Tx, A] with Sink[Tx, A]

  object VarR {
    def unapply[T <: Txn[T], A](view: CellView[T, A]): Option[VarR[T, A]] = view match {
      case vr: VarR[T, A] => Some(vr)
      case _              => None
    }
  }
  /** A mutable cell view representing serializable elements. */
  trait VarR[T <: Txn[T], A] extends Var[T, A] {
    type Repr

    def repr(implicit tx: T): Repr

    def repr_=(value: Repr)(implicit tx: T): Unit

    def lift(value: A)(implicit tx: T): Repr

    implicit def format: TFormat[T, Repr]
  }

  implicit final class Ops[Tx, A](private val in: CellView[Tx, A]) extends AnyVal {
    def map[B](f: A => B): CellView[Tx, B] =
      new MapImpl(in, f)
  }

  implicit final class OptionOps[Tx, A](private val in: CellView[Tx, Option[A]]) extends AnyVal {
    def orElse[B >: A](that: CellView[Tx, Option[B]]): CellView[Tx, Option[B]] =
      new OptionOrElseImpl(in, that)

    def flatMapTx[B](f: Tx => A => Option[B]): CellView[Tx, Option[B]] =
      new FlatMapImpl(in, f)
  }

  // ---- impl ----
  private object Empty extends CellView[Any, Option[Nothing]] {
    def react(fun: Any => Option[Nothing] => Unit)(implicit tx: Any): Disposable[Any] =
      Disposable.empty

    def apply()(implicit tx: Any): Option[Nothing] = None
  }
}

/** A `CellView` is an in-memory view of a transactional object that fires updates when the object changes.
 *
 * It is important that the cell-view is ''not'' a `Disposable`, which means we do not have to track its
 * life cycle. A `Disposable` of course is generated from the `react` call.
 */
trait CellView[Tx, +A] extends Observable[Tx, A] with Source[Tx, A]