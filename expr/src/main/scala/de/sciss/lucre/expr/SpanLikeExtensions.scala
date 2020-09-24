/*
 *  SpanLikeExtensions.scala
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

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{ExprTuple1, ExprTuple1Op, ExprTuple2, ExprTuple2Op}
import de.sciss.lucre.{Copy, Elem, Expr, LongObj, Obj, SpanLikeObj, Txn}
import de.sciss.serial.DataInput
import de.sciss.span.{Span, SpanLike}

import scala.annotation.switch

object SpanLikeExtensions {
  private[this] lazy val _init: Unit = {
    SpanLikeObj.registerExtension(SpanLikeTuple1s)
    SpanLikeObj.registerExtension(SpanLikeTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = SpanLikeObj[T]

  def newExpr[T <: Txn[T]](start: LongObj[T], stop: LongObj[T])(implicit tx: T): _Ex[T] =
    BinaryOp.Apply(start, stop)

  def from[T <: Txn[T]](start: LongObj[T])(implicit tx: T): _Ex[T] =
    UnaryOp.From(start)

  def until[T <: Txn[T]](stop: LongObj[T])(implicit tx: T): _Ex[T] =
    UnaryOp.Until(stop)

  private[this] object SpanLikeTuple1s extends Expr.Type.Extension1[SpanLikeObj] {
    // final val arity = 1
    final val opLo: Int = UnaryOp.From .id
    final val opHi: Int = UnaryOp.Until.id

    val name = "Long-SpanLike Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import UnaryOp._
      val op /* : Op[_, _] */ = (opId: @switch) match {
        case From .id => From
        case Until.id => Until
        case _ => sys.error(s"Invalid operation id $opId")
      }
      op.read(in, targets)
    }
  }

  private[this] object SpanLikeTuple2s extends Expr.Type.Extension1[SpanLikeObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Apply.id
    final val opHi: Int = BinaryOp.Shift.id

    val name = "Long-SpanLike Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import BinaryOp._
      val op /* : Op[_, _, _, _] */ = opId /* : @switch */ match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read(in, targets)
    }
  }

  final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](
                                                                           protected val targets: Targets[T], val op: ExprTuple1Op[SpanLike, T1, SpanLikeObj, ReprT1], val _1: ReprT1[T])
    extends ExprTuple1[T, SpanLike, T1, SpanLikeObj, ReprT1] with SpanLikeObj[T] {

    def tpe: Obj.Type = SpanLikeObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple1(Targets[Out], op, context(_1)).connect()
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[SpanLike, T1, T2, SpanLikeObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, SpanLike, T1, T2, SpanLikeObj, ReprT1, ReprT2] with SpanLikeObj[T] {

    def tpe: Obj.Type = SpanLikeObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  // ---- operators ----

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => ex}
    // ---- binary ----
    def shift(delta: LongObj[T])(implicit tx: T): _Ex[T] = BinaryOp.Shift(ex, delta)
  }

  private object UnaryOp {
    sealed trait Op[T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]] {
      def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                           (implicit tx: T): _Ex[T]

      def toString[T <: Txn[T]](_1: ReprT1[T]): String = s"$name(${_1})"

      def name: String = {
        val cn  = getClass.getName
        val sz  = cn.length
        val i   = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongOp extends ExprTuple1Op[SpanLike, Long, SpanLikeObj, LongObj]
      with Op[Long, LongObj] {

      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): _Ex[T] = {
        val _1 = LongObj.read(in)
        new Tuple1[T, Long, LongObj](targets, this, _1)
      }

      final def apply[T <: Txn[T]](a: LongObj[T])(implicit tx: T): _Ex[T] =
        new Tuple1[T, Long, LongObj](Targets[T], this, a).connect()
    }

    case object From extends LongOp {
      final val id = 0
      def value(a: Long): SpanLike = Span.from(a)

      override def toString[T <: Txn[T]](_1: LongObj[T]): String = s"Span.from(${_1 })"
    }

    case object Until extends LongOp {
      final val id = 1
      def value(a: Long): SpanLike = Span.until(a)

      override def toString[T <: Txn[T]](_1: LongObj[T]): String = s"Span.until(${_1})"
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2, ReprT1[~ <: Txn[~]] <: Expr[~, T1], ReprT2[~ <: Txn[~]] <: Expr[~, T2]] {
      def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                           (implicit tx: T): _Ex[T]

      def toString[T <: Txn[T]](_1: ReprT1[T], _2: ReprT2[T]): String =
        s"${_1}.$name(${_2})"

      def value(a: T1, b: T2): SpanLike

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongSpanOp
      extends ExprTuple2Op[SpanLike, SpanLike, Long, SpanLikeObj, SpanLikeObj, LongObj]
        with Op[SpanLike, Long, SpanLikeObj, LongObj] {

      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): _Ex[T] = {
        val _1 = SpanLikeObj.read(in)
        val _2 = LongObj    .read(in)
        new Tuple2[T, SpanLike, SpanLikeObj, Long, LongObj](targets, this, _1, _2)
      }

      final def apply[T <: Txn[T]](a: _Ex[T], b: LongObj[T])(implicit tx: T): _Ex[T] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => SpanLikeObj.newConst(value(ca, cb))
        case _  =>
          new Tuple2[T, SpanLike, SpanLikeObj, Long, LongObj](Targets[T], this, a, b).connect()
      }
    }

    sealed abstract class LongLongOp
      extends ExprTuple2Op[SpanLike, Long, Long, SpanLikeObj, LongObj, LongObj]
        with Op[Long, Long, LongObj, LongObj] {

      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): _Ex[T] = {
        val _1 = LongObj.read(in)
        val _2 = LongObj.read(in)
        new Tuple2[T, Long, LongObj, Long, LongObj](targets, this, _1, _2)
      }

      final def apply[T <: Txn[T]](a: LongObj[T], b: LongObj[T])(implicit tx: T): _Ex[T] =
        new Tuple2[T, Long, LongObj, Long, LongObj](Targets[T], this, a, b).connect()
    }

    case object Apply extends LongLongOp {
      final val id = 2
      override def toString[T <: Txn[T]](_1: LongObj[T], _2: LongObj[T]): String =
        s"Span(${_1}, ${_2})"

      def value(a: Long, b: Long): SpanLike = Span(a, b)
    }

    case object Shift extends LongSpanOp {
      final val id = 3
      def value(a: SpanLike, b: Long): SpanLike = a.shift(b)
    }
  }
}