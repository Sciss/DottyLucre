/*
 *  IntExtensions.scala
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
import de.sciss.lucre.{BooleanObj, Copy, Elem, Expr, IntObj, Obj, Txn}
import de.sciss.serial.DataInput

import scala.annotation.switch

object IntExtensions {
  private[this] lazy val _init: Unit = {
    IntObj.registerExtension(IntTuple1s)
    IntObj.registerExtension(IntTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = IntObj[T]

  private[this] object IntTuple1s extends Expr.Type.Extension1[IntObj] {
    // final val arity = 1
    final val opLo: Int = Neg         .id
    final val opHi: Int = BooleanToInt.id

    val name = "Int-1 Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      val op /* : UnaryOp[_, _] */ = (opId: @switch) match {
        // ---- Int => Int ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
        // ---- Boolean => Int ----
        case BooleanToInt.id => BooleanToInt
      }
      op.read(in, targets)
    }
  }

  private[this] object IntTuple2s extends Expr.Type.Extension1[IntObj] {
    // final val arity = 2
    final val opLo: Int = Plus  .id
    final val opHi: Int = AbsDif.id

    val name = "Int-2 Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      val op: BinaryOp = (opId: @switch) match {
        case Plus               .id => Plus
        case Minus              .id => Minus
        case Times              .id => Times
        case IDiv               .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min                .id => Min
        case Max                .id => Max
        case BitAnd             .id => BitAnd
        case BitOr              .id => BitOr
        case BitXor             .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        case ShiftLeft          .id => ShiftLeft
        case ShiftRight         .id => ShiftRight
        case UnsignedShiftRight .id => UnsignedShiftRight
        case AbsDif             .id => AbsDif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = IntObj.read(in)
      val _2 = IntObj.read(in)
      new Tuple2[T, Int, IntObj, Int, IntObj](targets, op, _1, _2)
    }
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[Int, T1, T2, IntObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, Int, T1, T2, IntObj, ReprT1, ReprT2] with IntObj[T] {

    def tpe: Obj.Type = IntObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  // ---- operators ----

  sealed trait UnaryOp[T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]] extends ExprTuple1Op[Int, T1, IntObj, ReprT1] {
    def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                         (implicit tx: T): _Ex[T] //  ExprTuple1[T, Int, T1]

    def toString[T <: Txn[T]](_1: ReprT1[T]): String = s"${_1}.$name"

    def apply[T <: Txn[T]](a: ReprT1[T])(implicit tx: T): _Ex[T] = a match {
      case Expr.Const(c)  => IntObj.newConst[T](value(c))
      case _              => new Tuple1[T, T1, ReprT1](Targets[T], this, a).connect()
    }

    def name: String = {
      val cn  = getClass.getName
      val sz  = cn.length
      val i   = cn.lastIndexOf('$', sz - 2) + 1
      s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
    }
  }

  final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](
                                                                           protected val targets: Targets[T], val op: ExprTuple1Op[Int, T1, IntObj, ReprT1], val _1: ReprT1[T])
    extends ExprTuple1[T, Int, T1, IntObj, ReprT1] with IntObj[T] {

    def tpe: Obj.Type = IntObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out], op, context(_1)).connect()
  }

  // ---- Int => Int ----

  private[this] sealed abstract class IntUnaryOp extends UnaryOp[Int, IntObj] {
    final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                               (implicit tx: T): _Ex[T] = {
      val _1 = IntObj.read(in)
      new Tuple1[T, Int, IntObj](targets, this, _1)
    }
  }

  private[this] case object Neg extends IntUnaryOp {
    final val id = 0
    def value(a: Int): Int = -a
    override def toString[T <: Txn[T]](_1: _Ex[T]): String = s"-${_1}"
  }

  private[this] case object Abs extends IntUnaryOp {
    final val id = 1
    def value(a: Int): Int = math.abs(a)
  }

  private[this] case object BitNot extends IntUnaryOp {
    final val id = 2
    def value(a: Int): Int = ~a
    override def toString[T <: Txn[T]](_1: _Ex[T]): String = s"~${_1}"
  }

  // case object ToLong     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  private[this] case object Signum extends IntUnaryOp {
    final val id = 3
    def value(a: Int): Int = math.signum(a)
  }

  private[this] case object Squared extends IntUnaryOp {
    final val id = 4
    def value(a: Int): Int = a * a
  }

  private[this] case object Cubed extends IntUnaryOp {
    final val id = 5
    def value(a: Int): Int = a * a * a
  }

  // ---- Boolean => Int ----

  sealed trait BooleanUnaryOp extends UnaryOp[Boolean, BooleanObj] {
    final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                               (implicit tx: T): _Ex[T] = {
      val _1 = BooleanObj.read(in)
      new Tuple1[T, Boolean, BooleanObj](targets, this, _1)
    }
  }

  case object BooleanToInt extends BooleanUnaryOp {
    final val id = 6
    def value(a: Boolean): Int = if (a) 1 else 0
  }

  // ---- (Int, Int) => Int ----

  private[this] sealed trait BinaryOp extends ExprTuple2Op[Int, Int, Int, IntObj, IntObj, IntObj] {
    final def apply[T <: Txn[T]](a: _Ex[T], b: _Ex[T])(implicit tx: T): _Ex[T] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => IntObj.newConst(value(ca, cb))
      case _ =>
        new Tuple2[T, Int, IntObj, Int, IntObj](Targets[T], this,  a, b).connect()
    }

    def value(a: Int, b: Int): Int

    def isInfix: Boolean

    final def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String =
      if (isInfix) s"(${_1} $name ${_2})" else s"${_1}.$name(${_2})"

    def name: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
    }
  }

  private[this] case object Plus extends BinaryOp {
    final val id = 7
    override val name = "+"
    def value(a: Int, b: Int): Int = a + b
    final val isInfix = true
  }

  private[this] case object Minus extends BinaryOp {
    final val id = 8
    override val name = "-"
    def value(a: Int, b: Int): Int = a - b
    final val isInfix = true
  }

  private[this] case object Times extends BinaryOp {
    final val id = 9
    override val name = "*"
    def value(a: Int, b: Int): Int = a * b
    final val isInfix = true
  }

  private[this] case object IDiv extends BinaryOp {
    final val id = 10
    override val name = "div"
    def value(a: Int, b: Int): Int = a / b
    val isInfix = false
  }

  private[this] case object Min extends BinaryOp {
    final val id = 11
    def value(a: Int, b: Int): Int = math.min(a, b)
    val isInfix = false
  }

  private[this] case object Max extends BinaryOp {
    final val id = 12
    def value(a: Int, b: Int): Int = math.max(a, b)
    val isInfix = false
  }

  private[this] case object BitAnd extends BinaryOp {
    final val id = 13
    def value(a: Int, b: Int): Int = a & b
    val isInfix = false
  }

  private[this] case object BitOr extends BinaryOp {
    final val id = 14
    def value(a: Int, b: Int): Int = a | b
    val isInfix = false
  }

  private[this] case object BitXor extends BinaryOp {
    final val id = 15
    def value(a: Int, b: Int): Int = a ^ b
    val isInfix = false
  }

  private[this] case object ShiftLeft extends BinaryOp {
    final val id = 16
    override val name = "<<"
    def value(a: Int, b: Int): Int = a << b
    val isInfix = false
  }

  private[this] case object ShiftRight extends BinaryOp {
    final val id = 17
    override val name = ">>"
    def value(a: Int, b: Int): Int = a >> b
    val isInfix = false
  }

  private[this] case object UnsignedShiftRight extends BinaryOp {
    final val id = 18
    override val name = ">>>"
    def value(a: Int, b: Int): Int = a >>> b
    val isInfix = false
  }

  private[this] case object AbsDif extends BinaryOp {
    final val id = 19
    def value(a: Int, b: Int): Int = math.abs(a - b)
    val isInfix = false
  }

  //      case object Clip2          extends Op( 42 ) {
  //         def value( a: Int, b: Int ) : Int = ri_clip2( a, b )
  //      }
  //      case object Fold2          extends Op( 44 ) {
  //         def value( a: Int, b: Int ) : Int = ri_fold2( a, b )
  //      }
  //      case object Wrap2          extends Op( 45 ) {
  //         def value( a: Int, b: Int ) : Int = ri_wrap2( a, b )
  //      }

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => a}

    private type E = _Ex[T]

    // ---- Int => Int ----

    def unary_- (implicit tx: T): E = Neg   (a)
    def unary_~ (implicit tx: T): E = BitNot(a)

    // ---- (Int, Int) => Int ----

    def +   (b: E)(implicit tx: T): E = Plus              (a, b)
    def -   (b: E)(implicit tx: T): E = Minus             (a, b)
    def *   (b: E)(implicit tx: T): E = Times             (a, b)
    def /   (b: E)(implicit tx: T): E = IDiv              (a, b)
    def &   (b: E)(implicit tx: T): E = BitAnd            (a, b)
    def |   (b: E)(implicit tx: T): E = BitOr             (a, b)
    def ^   (b: E)(implicit tx: T): E = BitXor            (a, b)
    def <<  (b: E)(implicit tx: T): E = ShiftLeft         (a, b)
    def >>  (b: E)(implicit tx: T): E = ShiftRight        (a, b)
    def >>> (b: E)(implicit tx: T): E = UnsignedShiftRight(a, b)

    // ---- (Int, Int) => Boolean ----

    def sig_==(b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntEq (a, b)
    def sig_!=(b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntNeq(a, b)
    def <     (b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntLt (a, b)
    def >     (b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntGt (a, b)
    def <=    (b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntLeq(a, b)
    def >=    (b: E)(implicit tx: T): BooleanObj[T] = BooleanExtensions.IntGeq(a, b)

    // ---- more ops ----

    def abs     (implicit tx: T): E = Abs     (a)
    // def toLong : E	         = UnOp.make( 'asLong, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )
    def signum  (implicit tx: T): E = Signum  (a)
    def squared (implicit tx: T): E = Squared (a)
    def cubed   (implicit tx: T): E = Cubed   (a)

    def min   (b: E)(implicit tx: T): E = Min   (a, b)
    def max   (b: E)(implicit tx: T): E = Max   (a, b)
    def absDif(b: E)(implicit tx: T): E = AbsDif(a, b)

    //      def clip2( b: E ) : E      = Clip2.make( ex, b )
    //      def fold2( b: E ) : E      = Fold2.make( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2.make( ex, b )
  }
}