/*
 *  SpanExtensions.scala
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
import de.sciss.lucre.impl.{ExprTuple2, ExprTuple2Op}
import de.sciss.lucre.{Copy, Elem, Expr, LongObj, Obj, SpanObj, Txn}
import de.sciss.serial.DataInput
import de.sciss.span.Span

import scala.annotation.switch

object SpanExtensions  {
  private[this] lazy val _init: Unit = {
    LongObj.registerExtension(LongTuple1s)
    SpanObj.registerExtension(SpanTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = SpanObj[T]

  private[this] object LongTuple1s extends Expr.Type.Extension1[LongObj] {
    // final val arity = 1
    final val opLo: Int = UnaryOp.Start .id
    final val opHi: Int = UnaryOp.Length.id

    val name = "Span-Long Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): LongObj[T] = {
      import UnaryOp._
      val op: LongOp = (opId: @switch) match {
        // ---- Span ----
        case Start  .id => Start
        case Stop   .id => Stop
        case Length .id => Length
      }
      op.read(in, targets)
    }
  }

  private[this] object SpanTuple2s extends Expr.Type.Extension1[SpanObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Apply.id
    final val opHi: Int = BinaryOp.Shift.id

    val name = "Int-Int Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import BinaryOp._
      val op /* : Op[_, _, _, _] */ = opId /* : @switch */ match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read[T](in, targets)
    }
  }

  final class Ops2(val `this`: Span.type) extends AnyVal { me =>
    // import me.{`this` => ex}
    def apply[T <: Txn[T]](start: LongObj[T], stop: LongObj[T])(implicit tx: T): _Ex[T] =
      (start, stop) match {
        case (Expr.Const(startC), Expr.Const(stopC)) => SpanObj.newConst(Span(startC, stopC))
        case _ =>
          new Tuple2[T, Long, LongObj, Long, LongObj](Targets[T](), BinaryOp.Apply, start, stop).connect()
      }
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[Span, T1, T2, SpanObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, Span, T1, T2, SpanObj, ReprT1, ReprT2] with SpanObj[T] {

    def tpe: Obj.Type = SpanObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out](), op, context(_1), context(_2)).connect()
  }

  // XXX TODO: fold constants
  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => ex}
    // ---- unary ----
    def start (implicit tx: T): LongObj[T] = UnaryOp.Start (ex)
    def stop  (implicit tx: T): LongObj[T] = UnaryOp.Stop  (ex)
    def length(implicit tx: T): LongObj[T] = UnaryOp.Length(ex)

    // ---- binary ----
    def shift(delta: LongObj[T])(implicit tx: T): _Ex[T] = BinaryOp.Shift(ex, delta)
  }

  // ----- operators -----

  object UnaryOp {
    //      sealed trait OpLike[ T1 ] {
    //         def toString[ S <: Txn[ S ]]( _1: Expr[ S, T1 ]) : String = _1.toString + "." + name
    //
    //         def name: String = { val cn = getClass.getName
    //            val sz   = cn.length
    //            val i    = cn.indexOf( '$' ) + 1
    //            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
    //         }
    //      }

    sealed abstract class LongOp extends LongExtensions.UnaryOp.Op[Span, SpanObj] /* Longs.UnaryOp.Op[Span] */ {
      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): LongObj[T] = {
        val _1 = SpanObj.read[T](in)
        new LongExtensions.Tuple1[T, Span, SpanObj](targets, this, _1)
      }
    }

    //    private[this] final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](
    //        protected val targets: Targets[T], val op: ExprTuple1Op[Span, T1, SpanObj, ReprT1], val _1: ReprT1[T])
    //      extends ExprTuple1[T, Span, T1, SpanObj, ReprT1] with SpanObj[T] {
    //
    //      def tpe: Obj.Type = SpanObj
    //    }

    case object Start extends LongOp {
      final val id = 20
      def value(a: Span): Long = a.start
    }

    case object Stop extends LongOp {
      final val id = 21
      def value(a: Span): Long = a.stop
    }

    case object Length extends LongOp {
      final val id = 22
      def value(a: Span): Long = a.length
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2, ReprT1[~ <: Txn[~]] <: Expr[~, T1], ReprT2[~ <: Txn[~]] <: Expr[~, T2]] {
      def toString[T <: Txn[T]](_1: ReprT1[T], _2: ReprT2[T]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }

      def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                           (implicit tx: T): SpanObj[T] // ExprTuple2[T, Span, T1, T2]
    }

    sealed abstract class LongSpanOp(val id: Int)
      extends ExprTuple2Op[Span, Span, Long, SpanObj, SpanObj, LongObj]
        with Op[Span, Long, SpanObj, LongObj] {

      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): _Ex[T] = {
        val _1 = SpanObj.read[T](in)
        val _2 = LongObj.read[T](in)
        new Tuple2[T, Span, SpanObj, Long, LongObj](targets, this, _1, _2)
      }

      final def apply[T <: Txn[T]](a: _Ex[T], b: LongObj[T])(implicit tx: T): _Ex[T] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => SpanObj.newConst[T](value(ca, cb))
        case _ =>
          new Tuple2[T, Span, SpanObj, Long, LongObj](Targets[T](), this, a, b).connect()
      }
    }

    object Apply extends ExprTuple2Op[Span, Long, Long, SpanObj, LongObj, LongObj]
      with Op[Long, Long, LongObj, LongObj] {

      final val id = 0

      def value(a: Long, b: Long): Span = Span(a, b)

      override def toString[T <: Txn[T]](_1: LongObj[T], _2: LongObj[T]): String = s"Span(${_1}, ${_2})"

      def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                           (implicit tx: T): _Ex[T] = {
        val _1 = LongObj.read(in)
        val _2 = LongObj.read(in)
        new Tuple2[T, Long, LongObj, Long, LongObj](targets, this, _1, _2)
      }
    }

    case object Shift extends LongSpanOp(1) {
      def value(a: Span, b: Long): Span = a.shift(b)
    }
  }
}