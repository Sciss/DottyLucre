/*
 *  DoubleExtensions.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{ExprTuple1, ExprTuple1Op, ExprTuple2, ExprTuple2Op}
import de.sciss.lucre.{Copy, DoubleObj, Elem, Expr, Obj, Txn}
import de.sciss.serial.DataInput

import scala.annotation.switch

object DoubleExtensions {
  private[this] lazy val _init: Unit = {
    DoubleObj.registerExtension(DoubleTuple1s)
    DoubleObj.registerExtension(DoubleTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = DoubleObj[T]

  private[this] object DoubleTuple1s extends Expr.Type.Extension1[DoubleObj] {
    // final val arity = 1
    final val opLo: Int = UnaryOp.Neg .id
    final val opHi: Int = UnaryOp.Tanh.id

    val name = "Double-Double Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import UnaryOp._
      val op: Op = (opId: @switch) match {
        case Neg        .id => Neg
        case Abs        .id => Abs
        case Ceil       .id => Ceil
        case Floor      .id => Floor
        case Frac       .id => Frac
        case Signum     .id => Signum
        case Squared    .id => Squared
        // case Cubed      .id => Cubed
        case Sqrt       .id => Sqrt
        case Exp        .id => Exp
        case Reciprocal .id => Reciprocal
        case MidiCps    .id => MidiCps
        case CpsMidi    .id => CpsMidi
        case MidiRatio  .id => MidiRatio
        case RatioMidi  .id => RatioMidi
        case DbAmp      .id => DbAmp
        case AmpDb      .id => AmpDb
        case OctCps     .id => OctCps
        case CpsOct     .id => CpsOct
        case Log        .id => Log
        case Log2       .id => Log2
        case Log10      .id => Log10
        case Sin        .id => Sin
        case Cos        .id => Cos
        case Tan        .id => Tan
        case Asin       .id => Asin
        case Acos       .id => Acos
        case Atan       .id => Atan
        case Sinh       .id => Sinh
        case Cosh       .id => Cosh
        case Tanh       .id => Tanh
      }
      val _1 = DoubleObj.read(in)
      new Tuple1[T, Double, DoubleObj](targets, op, _1)
    }
  }

  final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](
                                                                           protected val targets: Targets[T], val op: ExprTuple1Op[Double, T1, DoubleObj, ReprT1], val _1: ReprT1[T])
    extends ExprTuple1[T, Double, T1, DoubleObj, ReprT1] with DoubleObj[T] {

    def tpe: Obj.Type = DoubleObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out](), op, context(_1)).connect()
  }

  private[this] object DoubleTuple2s extends Expr.Type.Extension1[DoubleObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Plus .id
    final val opHi: Int = BinaryOp.Wrap2.id

    val name = "Double-Double Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import BinaryOp._
      val op: Op = (opId: @switch) match {
        case Plus   .id => Plus
        case Minus  .id => Minus
        case Times  .id => Times
        //      case 3 => IDiv
        case Div    .id => Div
        case Mod    .id => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min    .id => Min
        case Max    .id => Max
        //      case 14 => BitAnd
        //      case 15 => BitOr
        //      case 16 => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        case RoundTo  .id => RoundTo
        case RoundUpTo.id => RoundUpTo
        case Trunc  .id => Trunc
        case Atan2  .id => Atan2
        case Hypot  .id => Hypot
        case HypotApx .id => HypotApx
        case Pow    .id => Pow
        // case 26 => <<
        // case 27 => >>
        // case 28 => UnsgnRghtShft
        // case 29 => Fill
        //      case 30 => Ring1
        //      case 31 => Ring2
        //      case 32 => Ring3
        //      case 33 => Ring4
        case DifSqr .id => DifSqr
        case SumSqr .id => SumSqr
        case SqrSum .id => SqrSum
        case SqrDif .id => SqrDif
        case AbsDif .id => AbsDif
        // case Thresh .id => Thresh
        //      case 40 => Amclip
        //      case 41 => Scaleneg
        case Clip2.id => Clip2
        //      case 43 => Excess
        case Fold2.id => Fold2
        case Wrap2.id => Wrap2
      }
      val _1 = DoubleObj.read(in)
      val _2 = DoubleObj.read(in)
      new Tuple2[T, Double, DoubleObj, Double, DoubleObj](targets, op, _1, _2)
    }
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[Double, T1, T2, DoubleObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, Double, T1, T2, DoubleObj, ReprT1, ReprT2] with DoubleObj[T] {

    def tpe: Obj.Type = DoubleObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out](), op, context(_1), context(_2)).connect()
  }

  // ----- operators -----

  private object UnaryOp {
    import de.sciss.numbers.{DoubleFunctions => rd}

    sealed abstract class Op extends ExprTuple1Op[Double, Double, DoubleObj, DoubleObj] {
      def id: Int
      final def apply[T <: Txn[T]](_1: _Ex[T])(implicit tx: T): _Ex[T] = _1 match {
        case Expr.Const(c)  => DoubleObj.newConst(value(c))
        case _              => new Tuple1[T, Double, DoubleObj](Targets[T](), this, _1).connect()
      }

      def toString[T <: Txn[T]](_1: _Ex[T]): String = s"${_1}.$name"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    case object Neg extends Op {
      final val id = 0
      def value(a: Double): Double = -a // rd.neg(a)

      override def toString[T <: Txn[T]](_1: _Ex[T]): String = s"-${_1}"
    }

    case object Abs extends Op {
      final val id = 1
      def value(a: Double): Double = rd.abs(a)
    }

    // case object ToDouble     extends Op(  6 )
    // case object ToInt       extends Op(  7 )
    case object Ceil extends Op {
      final val id = 2
      def value(a: Double): Double = rd.ceil(a)
    }

    case object Floor extends Op {
      final val id = 3
      def value(a: Double): Double = rd.floor(a)
    }

    case object Frac extends Op {
      final val id = 4
      def value(a: Double): Double = rd.frac(a)
    }

    case object Signum extends Op {
      final val id = 5
      def value(a: Double): Double = rd.signum(a)
    }

    case object Squared extends Op {
      final val id = 6
      def value(a: Double): Double = rd.squared(a)
    }

    //    case object Cubed extends Op {
    //      final val id = 13
    //      def value(a: Double): Double = rd.cubed(a)
    //    }

    case object Sqrt extends Op {
      final val id = 7
      def value(a: Double): Double = rd.sqrt(a)
    }

    case object Exp extends Op {
      final val id = 8
      def value(a: Double): Double = rd.exp(a)
    }

    case object Reciprocal extends Op {
      final val id = 9
      def value(a: Double): Double = 1.0 / a // rd.reciprocal(a)
    }

    case object MidiCps extends Op {
      final val id = 10
      def value(a: Double): Double = rd.midiCps(a)
    }

    case object CpsMidi extends Op {
      final val id = 11
      def value(a: Double): Double = rd.cpsMidi(a)
    }

    case object MidiRatio extends Op {
      final val id = 12
      def value(a: Double): Double = rd.midiRatio(a)
    }

    case object RatioMidi extends Op {
      final val id = 13
      def value(a: Double): Double = rd.ratioMidi(a)
    }

    case object DbAmp extends Op {
      final val id = 14
      def value(a: Double): Double = rd.dbAmp(a)
    }

    case object AmpDb extends Op {
      final val id = 15
      def value(a: Double): Double = rd.ampDb(a)
    }

    case object OctCps extends Op {
      final val id = 16
      def value(a: Double): Double = rd.octCps(a)
    }

    case object CpsOct extends Op {
      final val id = 17
      def value(a: Double): Double = rd.cpsOct(a)
    }

    case object Log extends Op {
      final val id = 18
      def value(a: Double): Double = rd.log(a)
    }

    case object Log2 extends Op {
      final val id = 19
      def value(a: Double): Double = rd.log2(a)
    }

    case object Log10 extends Op {
      final val id = 20
      def value(a: Double): Double = rd.log10(a)
    }

    case object Sin extends Op {
      final val id = 21
      def value(a: Double): Double = rd.sin(a)
    }

    case object Cos extends Op {
      final val id = 22
      def value(a: Double): Double = rd.cos(a)
    }

    case object Tan extends Op {
      final val id = 23
      def value(a: Double): Double = rd.tan(a)
    }

    case object Asin extends Op {
      final val id = 24
      def value(a: Double): Double = rd.asin(a)
    }

    case object Acos extends Op {
      final val id = 25
      def value(a: Double): Double = rd.acos(a)
    }

    case object Atan extends Op {
      final val id = 26
      def value(a: Double): Double = rd.atan(a)
    }

    case object Sinh extends Op {
      final val id = 27
      def value(a: Double): Double = rd.sinh(a)
    }

    case object Cosh extends Op {
      final val id = 28
      def value(a: Double): Double = rd.cosh(a)
    }

    case object Tanh extends Op {
      final val id = 29
      def value(a: Double): Double = rd.tanh(a)
    }

    // class Rand              extends Op( 37 )
    // class Rand2             extends Op( 38 )
    // class Linrand           extends Op( 39 )
    // class Bilinrand         extends Op( 40 )
    // class Sum3rand          extends Op( 41 )
    // case object Distort     extends Op( 42 )
    // case object Softclip    extends Op( 43 )
    // class Coin              extends Op( 44 )
    // case object DigitValue  extends Op( 45 )
    // case object Silence     extends Op( 46 )
    // case object Thru        extends Op( 47 )
    // case object RectWindow  extends Op( 48 )
    // case object HanWindow   extends Op( 49 )
    // case object WelWindow   extends Op( 50 )
    // case object TriWindow   extends Op( 51 )
    // case object Ramp        extends Op( 52 )
    // case object Scurve      extends Op( 53 )
  }

  private object BinaryOp {
    import de.sciss.numbers.{DoubleFunctions => rd}

    sealed abstract class Op extends ExprTuple2Op[Double, Double, Double, DoubleObj, DoubleObj, DoubleObj] {

      final def apply[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T])(implicit tx: T): _Ex[T] = (_1, _2) match {
        case (Expr.Const(ca), Expr.Const(cb)) => DoubleObj.newConst(value(ca, cb))
        case _ =>
          new Tuple2[T, Double, DoubleObj, Double, DoubleObj](Targets[T](), this, _1, _2).connect()
      }

      def value(a: Double, b: Double): Double

      def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    trait Infix {
      _: Op =>

      override def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String =
        s"(${_1} $name ${_2})"
    }

    case object Plus extends Op with Infix {
      final val id = 30
      override val name = "+"

      def value(a: Double, b: Double): Double = rd.+(a, b)
    }

    case object Minus extends Op with Infix {
      final val id = 31
      override val name = "-"

      def value(a: Double, b: Double): Double = rd.-(a, b)
    }

    case object Times extends Op with Infix {
      final val id = 32
      override val name = "*"

      def value(a: Double, b: Double): Double = rd.*(a, b)
    }

    //      case object IDiv           extends Op(  3 ) {
    //         override val name = "div"
    //         protected def make1( a: Double, b: Double ) : Int = rd.div( a, b )
    //      }
    case object Div extends Op with Infix {
      final val id = 33
      override val name = "/"

      def value(a: Double, b: Double): Double = rd./(a, b)
    }

    case object Mod extends Op with Infix {
      final val id = 34
      override val name = "%"

      def value(a: Double, b: Double): Double = rd.%(a, b)
    }

    //      case object Eq             extends Op(  6 )
    //      case object Neq            extends Op(  7 )
    //      case object Lt             extends Op(  8 )
    //      case object Gt             extends Op(  9 )
    //      case object Leq            extends Op( 10 )
    //      case object Geq            extends Op( 11 )
    case object Min extends Op {
      final val id = 35
      def value(a: Double, b: Double): Double = rd.min(a, b)
    }

    case object Max extends Op {
      final val id = 36
      def value(a: Double, b: Double): Double = rd.max(a, b)
    }

    //      case object BitAnd         extends Op( 14 )
    //      case object BitOr          extends Op( 15 )
    //      case object BitXor         extends Op( 16 )
    // case object Lcm            extends Op( 17 )
    // case object Gcd            extends Op( 18 )
    case object RoundTo extends Op {
      final val id = 37
      def value(a: Double, b: Double): Double = rd.roundTo(a, b)
    }

    case object RoundUpTo extends Op {
      final val id = 38
      def value(a: Double, b: Double): Double = rd.roundUpTo(a, b)
    }

    case object Trunc extends Op {
      final val id = 39
      def value(a: Double, b: Double): Double = rd.trunc(a, b)
    }

    case object Atan2 extends Op {
      final val id = 40
      def value(a: Double, b: Double): Double = rd.atan2(a, b)
    }

    case object Hypot extends Op {
      final val id = 41
      def value(a: Double, b: Double): Double = rd.hypot(a, b)
    }

    case object HypotApx extends Op {
      final val id = 42
      def value(a: Double, b: Double): Double = rd.hypotApx(a, b)
    }

    case object Pow extends Op {
      final val id = 43
      def value(a: Double, b: Double): Double = rd.pow(a, b)
    }

    // case object <<             extends Op( 26 )
    // case object >>             extends Op( 27 )
    // case object UnsgnRghtShft  extends Op( 28 )
    // case object Fill           extends Op( 29 )
    //      case object Ring1          extends Op( 30 )
    //      case object Ring2          extends Op( 31 )
    //      case object Ring3          extends Op( 32 )
    //      case object Ring4          extends Op( 33 )
    case object DifSqr extends Op {
      final val id = 44
      def value(a: Double, b: Double): Double = rd.difSqr(a, b)
    }

    case object SumSqr extends Op {
      final val id = 45
      def value(a: Double, b: Double): Double = rd.sumSqr(a, b)
    }

    case object SqrSum extends Op {
      final val id = 46
      def value(a: Double, b: Double): Double = rd.sqrSum(a, b)
    }

    case object SqrDif extends Op {
      final val id = 47
      def value(a: Double, b: Double): Double = rd.sqrDif(a, b)
    }

    case object AbsDif extends Op {
      final val id = 48
      def value(a: Double, b: Double): Double = rd.absDif(a, b)
    }

    //    case object Thresh extends Op {
    //      final val id = 39
    //      def value(a: Double, b: Double): Double = rd.thresh(a, b)
    //    }

    //      case object Amclip         extends Op( 40 )
    //      case object Scaleneg       extends Op( 41 )
    case object Clip2 extends Op {
      final val id = 49
      def value(a: Double, b: Double): Double = rd.clip2(a, b)
    }

    //      case object Excess         extends Op( 43 )
    case object Fold2 extends Op {
      final val id = 50
      def value(a: Double, b: Double): Double = rd.fold2(a, b)
    }

    case object Wrap2 extends Op {
      final val id = 51
      def value(a: Double, b: Double): Double = rd.wrap2(a, b)
    }

    //      case object Firstarg       extends Op( 46 )
  }

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = _Ex[T]

    import UnaryOp._

    def unary_- (implicit tx: T): E = Neg(a)

    // def bitNot : E	         = BitNot.make( ex )
    // def toDouble : E	         = UnOp.make( 'asDouble, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )

    import BinaryOp._

    def + (b: E)(implicit tx: T): E = Plus (a, b)
    def - (b: E)(implicit tx: T): E = Minus(a, b)
    def * (b: E)(implicit tx: T): E = Times(a, b)
    def / (b: E)(implicit tx: T): E = Div  (a, b)

    import UnaryOp._

    def abs       (implicit tx: T): E = Abs       (a)
    def ceil      (implicit tx: T): E = Ceil      (a)
    def floor     (implicit tx: T): E = Floor     (a)
    def frac      (implicit tx: T): E = Frac      (a)
    def signum    (implicit tx: T): E = Signum    (a)
    def squared   (implicit tx: T): E = Squared   (a)
    // def cubed     : E = Cubed     (ex)
    def sqrt      (implicit tx: T): E = Sqrt      (a)
    def exp       (implicit tx: T): E = Exp       (a)
    def reciprocal(implicit tx: T): E = Reciprocal(a)
    def midiCps   (implicit tx: T): E = MidiCps   (a)
    def cpsMidi   (implicit tx: T): E = CpsMidi   (a)
    def midiRatio (implicit tx: T): E = MidiRatio (a)
    def ratioMidi (implicit tx: T): E = RatioMidi (a)
    def dbAmp     (implicit tx: T): E = DbAmp     (a)
    def ampDb     (implicit tx: T): E = AmpDb     (a)
    def octCps    (implicit tx: T): E = OctCps    (a)
    def cpsOct    (implicit tx: T): E = CpsOct    (a)
    def log       (implicit tx: T): E = Log       (a)
    def log2      (implicit tx: T): E = Log2      (a)
    def log10     (implicit tx: T): E = Log10     (a)
    def sin       (implicit tx: T): E = Sin       (a)
    def cos       (implicit tx: T): E = Cos       (a)
    def tan       (implicit tx: T): E = Tan       (a)
    def asin      (implicit tx: T): E = Asin      (a)
    def acos      (implicit tx: T): E = Acos      (a)
    def atan      (implicit tx: T): E = Atan      (a)
    def sinh      (implicit tx: T): E = Sinh      (a)
    def cosh      (implicit tx: T): E = Cosh      (a)
    def tanh      (implicit tx: T): E = Tanh      (a)

    // def rand : E              = UnOp.make( 'rand, ex )
    // def rand2 : E             = UnOp.make( 'rand2, ex )
    // def linRand : E           = UnOp.make( 'linrand, ex )
    // def bilinRand : E         = UnOp.make( 'bilinrand, ex )
    // def sum3Rand : E          = UnOp.make( 'sum3rand, ex )
    // def distort : E   = Distort.make( ex )
    // def softClip : E  = Softclip.make( ex )
    // def coin : E              = UnOp.make( 'coin, ex )
    // def even : E              = UnOp.make( 'even, ex )
    // def odd : E               = UnOp.make( 'odd, ex )
    // def rectWindow : E        = UnOp.make( 'rectWindow, ex )
    // def hannWindow : E         = UnOp.make( 'hanWindow, ex )
    // def welchWindow : E         = UnOp.make( 'sum3rand, ex )
    // def triWindow : E         = UnOp.make( 'triWindow, ex )
    // def ramp : E      = Ramp.make( ex )
    // def sCurve : E    = Scurve.make( ex )
    // def isPositive : E        = UnOp.make( 'isPositive, ex )
    // def isNegative : E        = UnOp.make( 'isNegative, ex )
    // def isStrictlyPositive : E= UnOp.make( 'isStrictlyPositive, ex )
    // def rho : E               = UnOp.make( 'rho, ex )
    // def theta : E             = UnOp.make( 'theta, ex )

    import BinaryOp._

    def min     (b: E)(implicit tx: T): E = Min     (a, b)
    def max     (b: E)(implicit tx: T): E = Max     (a, b)
    def roundTo (b: E)(implicit tx: T): E = RoundTo (a, b)
    def roundUpTo(b: E)(implicit tx: T): E = RoundUpTo(a,b)
    def trunc   (b: E)(implicit tx: T): E = Trunc   (a, b)
    def atan2   (b: E)(implicit tx: T): E = Atan2   (a, b)
    def hypot   (b: E)(implicit tx: T): E = Hypot   (a, b)
    def hypotApx(b: E)(implicit tx: T): E = HypotApx  (a, b)
    def pow     (b: E)(implicit tx: T): E = Pow     (a, b)

    //      def ring1( b: E ) : E     = Ring1.make( ex, b )
    //      def ring2( b: E ) : E     = Ring2.make( ex, b )
    //      def ring3( b: E ) : E     = Ring3.make( ex, b )
    //      def ring4( b: E ) : E     = Ring4.make( ex, b )
    def difSqr  (b: E)(implicit tx: T): E = DifSqr  (a, b)
    def sumSqr  (b: E)(implicit tx: T): E = SumSqr  (a, b)
    def sqrSum  (b: E)(implicit tx: T): E = SqrSum  (a, b)
    def sqrDif  (b: E)(implicit tx: T): E = SqrDif  (a, b)
    def absDif  (b: E)(implicit tx: T): E = AbsDif  (a, b)
    // def thresh  (b: E): E = Thresh  (ex, b)

    //      def amClip( b: E ) : E    = Amclip.make( ex, b )
    //      def scaleNeg( b: E ) : E  = Scaleneg.make( ex, b )
    def clip2   (b: E)(implicit tx: T): E = Clip2   (a, b)

    //      def excess( b: E ) : E    = Excess.make( ex, b )
    def fold2   (b: E)(implicit tx: T): E = Fold2   (a, b)
    def wrap2   (b: E)(implicit tx: T): E = Wrap2   (a, b)

    // def firstArg( b: Double ) : Double  = d

    //      def linLin( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
    //         rd.linLin( d, srcLo, srcHi, dstLo, dstHi )
    //
    //      def linExp( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
    //         rd.linExp( d, srcLo, srcHi, dstLo, dstHi )
  }
}