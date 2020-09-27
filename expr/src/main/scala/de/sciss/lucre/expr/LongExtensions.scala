/*
 *  LongExtensions.scala
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

package de.sciss.lucre
package expr

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{ExprTuple1, ExprTuple1Op, ExprTuple2, ExprTuple2Op}
import de.sciss.serial.DataInput

import scala.annotation.switch

object LongExtensions {
  private[this] lazy val _init: Unit = {
    LongObj.registerExtension(LongTuple1s)
    LongObj.registerExtension(LongTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = LongObj[T]

  private[this] object LongTuple1s extends Expr.Type.Extension1[LongObj] {
    // final val arity = 1
    final val opLo: Int = UnaryOp.Neg  .id
    final val opHi: Int = UnaryOp.Cubed.id

    val name = "Long-Long Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import UnaryOp._
      val op /* : Op[_, _] */ = (opId: @switch) match {
        // ---- Long ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
      }
      op.read(in, targets)
    }
  }

  private[this] object LongTuple2s extends Expr.Type.Extension1[LongObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Plus  .id
    final val opHi: Int = BinaryOp.AbsDif.id

    val name = "Long-Long Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import BinaryOp._
      val op: Op = (opId: @switch) match {
        // ---- Long ----
        case Plus   .id => Plus
        case Minus  .id => Minus
        case Times  .id => Times
        case IDiv   .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min    .id => Min
        case Max    .id => Max
        case BitAnd .id => BitAnd
        case BitOr  .id => BitOr
        case BitXor .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        //               case 26 => <<
        //               case 27 => >>
        //               case 28 => >>>
        case AbsDif .id => AbsDif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = LongObj.read(in)
      val _2 = LongObj.read(in)
      new Tuple2[T, Long, LongObj, Long, LongObj](targets, op, _1, _2)
    }
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T],
                                             val op: ExprTuple2Op[Long, T1, T2, LongObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, Long, T1, T2, LongObj, ReprT1, ReprT2] with LongObj[T] {

    def tpe: Obj.Type = LongObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2(Targets[Out](), op, context(_1), context(_2)).connect()
  }

  final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](protected val targets: Targets[T],
                                                                          val op: ExprTuple1Op[Long, T1, LongObj, ReprT1],
                                                                          val _1: ReprT1[T])
    extends ExprTuple1[T, Long, T1, LongObj, ReprT1] with LongObj[T] {

    def tpe: Obj.Type = LongObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out](), op, context(_1)).connect()
  }

  object UnaryOp {
    trait Op[T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]] extends ExprTuple1Op[Long, T1, LongObj, ReprT1] {
      def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                           (implicit tx: T): _Ex[T]

      def toString[T <: Txn[T]](_1: ReprT1[T]): String = s"${_1}.$name"

      def apply[T <: Txn[T]](a: ReprT1[T])(implicit tx: T): _Ex[T] = a match {
        case Expr.Const(c)  => LongObj.newConst[T](value(c))
        case _              => new Tuple1[T, T1, ReprT1](Targets[T](), this, a).connect()
      }

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    sealed abstract class LongOp extends Op[Long, LongObj] {
      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): _Ex[T] = {
        val _1 = LongObj.read(in)
        new Tuple1[T, Long, LongObj](targets, this, _1)
      }
    }

    case object Neg extends LongOp {
      final val id = 0
      def value(a: Long): Long = -a
      override def toString[T <: Txn[T]](_1: _Ex[T]): String = s"-${_1}"
    }

    case object Abs extends LongOp {
      final val id = 1
      def value(a: Long): Long = math.abs(a)
    }

    case object BitNot extends LongOp {
      final val id = 2
      def value(a: Long): Long = ~a
      override def toString[T <: Txn[T]](_1: _Ex[T]): String = s"~${_1}"
    }

    // case object ToLong     extends Op(  6 )
    // case object ToInt       extends Op(  7 )

    case object Signum extends LongOp {
      final val id = 3
      def value(a: Long): Long = math.signum(a)
    }

    case object Squared extends LongOp {
      final val id = 4
      def value(a: Long): Long = a * a
    }

    case object Cubed extends LongOp {
      final val id = 5
      def value(a: Long): Long = a * a * a
    }
  }

  private object BinaryOp {
    sealed trait Op extends ExprTuple2Op[Long, Long, Long, LongObj, LongObj, LongObj] {
      final def apply[T <: Txn[T]](a: _Ex[T], b: _Ex[T])(implicit tx: T): _Ex[T] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => LongObj.newConst(value(ca, cb))
        case _ =>
          new Tuple2[T, Long, LongObj, Long, LongObj](Targets[T](), this, a, b).connect()
      }

      def value(a: Long, b: Long): Long

      def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.indexOf('$') + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    trait Infix {
      this: Op =>

      override def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String = s"(${_1} $name ${_2})"
    }

    case object Plus extends Op with Infix {
      final val id = 6
      override val name = "+"
      def value(a: Long, b: Long): Long = a + b
    }

    case object Minus extends Op with Infix {
      final val id = 7
      override val name = "-"
      def value(a: Long, b: Long): Long = a - b
    }

    case object Times extends Op with Infix {
      final val id = 8
      override val name = "*"
      def value(a: Long, b: Long): Long = a * b
    }

    case object IDiv extends Op {
      final val id = 9
      override val name = "div"
      def value(a: Long, b: Long): Long = a / b
    }

    case object Min extends Op {
      final val id = 10
      def value(a: Long, b: Long): Long = math.min(a, b)
    }

    case object Max extends Op {
      final val id = 11
      def value(a: Long, b: Long): Long = math.max(a, b)
    }

    case object BitAnd extends Op {
      final val id = 12
      def value(a: Long, b: Long): Long = a & b
    }

    case object BitOr extends Op {
      final val id = 13
      def value(a: Long, b: Long): Long = a | b
    }

    case object BitXor extends Op {
      final val id = 14
      def value(a: Long, b: Long): Long = a ^ b
    }

    //      case object <<             extends Op( 26 ) {
    //         def value( a: Long, b: Long ) : Long = a << b
    //      }
    //      case object >>             extends Op( 27 ) {
    //         def value( a: Long, b: Long ) : Long = a >> b
    //      }
    //      case object >>>            extends Op( 28 ) {
    //         def value( a: Long, b: Long ) : Long = a >>> b
    //      }
    case object AbsDif extends Op {
      final val id = 15
      def value(a: Long, b: Long): Long = math.abs(a - b)
    }

    //      case object Clip2          extends Op( 42 ) {
    //         def value( a: Long, b: Long ) : Long = rd_clip2( a, b )
    //      }
    //      case object Fold2          extends Op( 44 ) {
    //         def value( a: Long, b: Long ) : Long = rd_fold2( a, b )
    //      }
    //      case object Wrap2          extends Op( 45 ) {
    //         def value( a: Long, b: Long ) : Long = rd_wrap2( a, b )
    //      }
  }

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = _Ex[T]

    import UnaryOp._

    def unary_- (implicit tx: T): E = Neg   (a)
    def unary_~ (implicit tx: T): E = BitNot(a)

    import BinaryOp._

    def + (b: E)(implicit tx: T): E = Plus  (a, b)
    def - (b: E)(implicit tx: T): E = Minus (a, b)
    def * (b: E)(implicit tx: T): E = Times (a, b)
    def / (b: E)(implicit tx: T): E = IDiv  (a, b)
    def & (b: E)(implicit tx: T): E = BitAnd(a, b)
    def | (b: E)(implicit tx: T): E = BitOr (a, b)
    def ^ (b: E)(implicit tx: T): E = BitXor(a, b)

    def abs     (implicit tx: T): E = Abs     (a)

    // def toLong : E	         = UnOp( 'asLong, ex )
    // def toInteger : E	      = UnOp( 'asInteger, ex )
    def signum  (implicit tx: T): E = Signum  (a)
    def squared (implicit tx: T): E = Squared (a)
    def cubed   (implicit tx: T): E = Cubed   (a)

    import BinaryOp._

    def min   (b: E)(implicit tx: T): E = Min   (a, b)
    def max   (b: E)(implicit tx: T): E = Max   (a, b)
    def absDif(b: E)(implicit tx: T): E = AbsDif(a, b)

    //      def clip2( b: E ) : E      = Clip2( ex, b )
    //      def fold2( b: E ) : E      = Fold2( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2( ex, b )
  }
}