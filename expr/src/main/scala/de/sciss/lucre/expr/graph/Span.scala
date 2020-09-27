/*
 *  Span.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.{Context, graph}
import de.sciss.lucre.{IExpr, Txn}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

object Span {
  def apply(start: Ex[Long], stop: Ex[Long]): Ex[_Span] = Apply(start, stop)

  def all : Ex[_SpanLike] = All ()
  def void: Ex[_SpanLike] = Void()

  def from  (start: Ex[Long]): Ex[_SpanLike] = From (start)
  def until (stop : Ex[Long]): Ex[_SpanLike] = Until(stop)

  final case class All() extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$All" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(_Span.All)
  }

  final case class Void() extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$All" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(_Span.Void)
  }

  private[lucre] final case class ApplyOp() extends BinaryOp.Op[Long, Long, _Span] {
    override def productPrefix: String = s"Span$$ApplyOp" // serialization

    def apply(a: Long, b: Long): _Span = _Span(a, b)
  }

  private final case class Apply(start: Ex[Long], stop: Ex[Long]) extends Ex[_Span] {
    override def productPrefix: String = "Span" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _Span]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.BinaryOp.Expanded(ApplyOp(), start.expand[T], stop.expand[T], tx)
    }
  }

  private[lucre] final case class FromOp() extends UnaryOp.Op[Long, _SpanLike] {
    override def productPrefix: String = s"Span$$FromOp" // serialization

    def apply(a: Long): _SpanLike = _Span.From(a)
  }

  private[lucre] final case class UntilOp() extends UnaryOp.Op[Long, _SpanLike] {
    override def productPrefix: String = s"Span$$UntilOp" // serialization

    def apply(a: Long): _SpanLike = _Span.Until(a)
  }

  case class From(start: Ex[Long]) extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$From" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.UnaryOp.Expanded(FromOp(), start.expand[T], tx)
    }
  }

  case class Until(stop: Ex[Long]) extends Ex[_SpanLike] {
    override def productPrefix: String = s"Span$$Until" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _SpanLike]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new graph.UnaryOp.Expanded(UntilOp(), stop.expand[T], tx)
    }
  }
}
