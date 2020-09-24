/*
 *  Adjunct.scala
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

package de.sciss.lucre

import de.sciss.lucre.impl.{ScalarEqImpl, ScalarToNumImpl, SeqLikeEq, SeqLikeNum, SeqLikeNumDouble, SeqLikeNumFrac, SeqLikeToNum}
import de.sciss.numbers.{DoubleFunctions => rd, IntFunctions => ri, IntFunctions2 => ri2, LongFunctions => rl, LongFunctions2 => rl2}
import de.sciss.serial.{DataInput, DataOutput, Writable}

import scala.annotation.switch

/** These are basically our "type classes" with the ability to serialize and deserialize.
  * They are supported through `ProductHasAdjuncts` which is recognized in serialization.
  */
object Adjunct {
  private[Adjunct] final val COOKIE = 0x4175   // "Au"

  def read(in: DataInput): Adjunct = {
    val cookie  = in.readShort().toInt
    if (cookie != COOKIE) sys.error(s"Unexpected cookie - found ${cookie.toHexString}, expected ${COOKIE.toHexString}")
    val id      = in.readShort().toInt
    (id: @switch) match {
      case IntTop                   .id => IntTop
      case IntSeqTop                .id => IntSeqTop
      case DoubleTop                .id => DoubleTop
      case DoubleSeqTop             .id => DoubleSeqTop
      case BooleanTop               .id => BooleanTop
      case BooleanSeqTop            .id => BooleanSeqTop
      case LongTop                  .id => LongTop
      case StringTop                .id => StringTop
      case Widen.idIdentity             => Widen.identity[Any]
      case Widen .intSeqSeq         .id => Widen .intSeqSeq
      case Widen2.seqIntSeq         .id => Widen2.seqIntSeq
      case Widen .doubleSeqSeq      .id => Widen .doubleSeqSeq
      case Widen2.seqDoubleSeq      .id => Widen2.seqDoubleSeq
      case Widen .intDoubleDouble   .id => Widen .intDoubleDouble
      case Widen .longDoubleDouble  .id => Widen .longDoubleDouble
      case Widen2.doubleIntDouble   .id => Widen2.doubleIntDouble
      case Widen2.doubleLongDouble  .id => Widen2.doubleLongDouble
      case Widen2.longIntLong       .id => Widen2.longIntLong
      case WidenToDouble.DoubleImpl .id => WidenToDouble.DoubleImpl
      case _ =>
        val f = getFactory(id)
        f.readIdentifiedAdjunct(in)
    }
  }

  def readT[A <: Adjunct](in: DataInput): A = read(in).asInstanceOf[A]

  def write(out: DataOutput, adj: Adjunct): Unit = adj.write(out)

  trait WidenLowPriority {
    implicit object intSeqSeq extends Widen2[Int, Seq[Int], Seq[Int]] {
      def widen1(a: Int     ): Seq[Int] = a :: Nil
      def widen2(a: Seq[Int]): Seq[Int] = a

      final val id = 0x100
    }

    implicit object doubleSeqSeq extends Widen2[Double, Seq[Double], Seq[Double]] {
      def widen1(a: Double     ): Seq[Double] = a :: Nil
      def widen2(a: Seq[Double]): Seq[Double] = a

      final val id = 0x102
    }
  }

  trait WidenMidPriority extends WidenLowPriority {
    implicit object intDoubleDouble extends Widen2[Int, Double, Double] {
      def widen1(a: Int    ): Double = a.toDouble
      def widen2(a: Double ): Double = a

      final val id = 0x104
    }

    implicit object longDoubleDouble extends Widen2[Long, Double, Double] {
      def widen1(a: Long   ): Double = a.toDouble
      def widen2(a: Double ): Double = a

      final val id = 0x106
    }
  }

  object Widen extends WidenMidPriority {
    implicit def identity[A]: Widen2[A, A, A] = anyWiden.asInstanceOf[Identity[A]]

    private[Adjunct] final val idIdentity = 0xFF

    private val anyWiden = new Identity[Any]

    private final class Identity[A] extends Widen2[A, A, A] {
      def widen1(a: A): A = a
      def widen2(a: A): A = a

      def id: Int = idIdentity
    }
  }

  trait Widen[A1, A] extends Adjunct {
    def widen1(a: A1): A
  }

  object Widen2 {
    implicit object seqIntSeq extends Widen2[Seq[Int], Int, Seq[Int]] {
      def widen1(a: Seq[Int]): Seq[Int] = a
      def widen2(a: Int     ): Seq[Int] = a :: Nil

      final val id = 0x101
    }

    implicit object seqDoubleSeq extends Widen2[Seq[Double], Double, Seq[Double]] {
      def widen1(a: Seq[Double]): Seq[Double] = a
      def widen2(a: Double     ): Seq[Double] = a :: Nil

      final val id = 0x103
    }

    implicit object doubleIntDouble extends Widen2[Double, Int, Double] {
      def widen1(a: Double ): Double = a
      def widen2(a: Int    ): Double = a.toDouble

      final val id = 0x105
    }
    implicit object doubleLongDouble extends Widen2[Double, Long, Double] {
      def widen1(a: Double ): Double = a
      def widen2(a: Long   ): Double = a.toDouble

      final val id = 0x107
    }
    implicit object longIntLong extends Widen2[Long, Int, Long] {
      def widen1(a: Long ): Long = a
      def widen2(a: Int  ): Long = a.toLong

      final val id = 0x108
    }
  }

  trait Widen2[A1, A2, A] extends Widen[A1, A] {
    def widen1(a: A1): A
    def widen2(a: A2): A
  }

  trait EqLowPriority {
    implicit def intSeqTop   : IntSeqTop    .type = IntSeqTop
    implicit def doubleSeqTop: DoubleSeqTop .type = DoubleSeqTop
  }

  object Eq extends EqLowPriority {
    implicit def intTop   : IntTop    .type = IntTop
    implicit def doubleTop: DoubleTop .type = DoubleTop
    implicit def longTop  : LongTop   .type = LongTop
  }
  trait Eq[A] extends Adjunct {
    type Boolean

    def eq (a: A, b: A): Boolean
    def neq(a: A, b: A): Boolean
  }

  trait Ord[A] extends Eq[A] {
    def lt  (a: A, b: A): Boolean
    def lteq(a: A, b: A): Boolean
    def gt  (a: A, b: A): Boolean
    def gteq(a: A, b: A): Boolean
  }

  type ScalarOrd[A] = Ord[A] with Scalar[A]

  //  trait NumLowPriority {
  //    implicit def intTSeqop   : NumInt   [Seq[Int   ]] = IntSeqTop
  //    implicit def doubleSeqTop: NumDouble[Seq[Double]] = DoubleSeqTop
  //  }
  //
  //  object Num extends NumLowPriority {
  //    implicit def intTop   : NumInt   [Int   ] = IntTop
  //    implicit def doubleTop: NumDouble[Double] = DoubleTop
  //  }
  trait Num[A] extends Ord[A] {
    // binary
    def plus      (a: A, b: A): A
    def minus     (a: A, b: A): A
    def times     (a: A, b: A): A
    // standard `%` modulus
    def rem       (a: A, b: A): A
    def mod       (a: A, b: A): A
    def min       (a: A, b: A): A
    def max       (a: A, b: A): A
    def roundTo   (a: A, b: A): A
    def roundUpTo (a: A, b: A): A
    def trunc     (a: A, b: A): A
    //    def ring1     (a: A, b: A): A
    //    def ring2     (a: A, b: A): A
    //    def ring3     (a: A, b: A): A
    //    def ring4     (a: A, b: A): A
    def difSqr(a: A, b: A): A
    def sumSqr(a: A, b: A): A
    def sqrSum(a: A, b: A): A
    def sqrDif(a: A, b: A): A
    def absDif(a: A, b: A): A
    //    def thresh    (a: A, b: A): A
    //    def amclip    (a: A, b: A): A
    //    def scaleneg  (a: A, b: A): A
    def clip2     (a: A, b: A): A
    def excess    (a: A, b: A): A
    def fold2     (a: A, b: A): A
    def wrap2     (a: A, b: A): A

    // unary
    def negate (a: A): A
    def abs    (a: A): A
    def signum (a: A): A

    def squared   (a: A): A
    def cubed     (a: A): A

    def zero: A
    def one : A

    // random
    def rand [Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rand2[Tx](a: A      )(implicit r: Random[Tx], tx: Tx): A
    def rangeRand[Tx](a: A, b: A)(implicit r: Random[Tx], tx: Tx): A

    // ternary
    def fold(a: A, lo: A, hi: A): A
    def clip(a: A, lo: A, hi: A): A
    def wrap(a: A, lo: A, hi: A): A
  }

  type ScalarNum[A] = Num[A] with Scalar[A]

  trait NumFrac[A] extends Num[A] {
    def floor (a: A): A
    def ceil  (a: A): A
    def frac  (a: A): A

    def div   (a: A, b: A): A

    def reciprocal(a: A): A
  }

  type ScalarNumFrac[A] = NumFrac[A] with Scalar[A]

  trait NumLogic[A] extends Eq[A] {
    def and (a: A, b: A): A
    def or  (a: A, b: A): A
    def xor (a: A, b: A): A
  }

  trait NumBool[A] extends NumLogic[A] {
    def negate(a: A): A
  }

  type ScalarNumBool[A] = NumBool[A] with Scalar[A]

  trait NumInt[A] extends Num[A] with NumLogic[A] {
    def not   (a: A): A

    def and   (a: A, b: A): A
    def or    (a: A, b: A): A
    def xor   (a: A, b: A): A

    def lcm     (a: A, b: A): A
    def gcd     (a: A, b: A): A

    def shiftLeft         (a: A, b: A): A
    def shiftRight        (a: A, b: A): A
    def unsignedShiftRight(a: A, b: A): A

    def div     (a: A, b: A): A
  }

  type ScalarNumInt[A] = NumInt[A] with Scalar[A]

  //  trait NumDoubleLowPriority {
  //    implicit def doubleSeqTop: NumDouble[Seq[Double]] = DoubleSeqTop
  //  }
  //
  //  object NumDouble {
  //    implicit def doubleTop: NumDouble[Double] = DoubleTop
  //  }

  object WidenToDouble {
    //    implicit def double     : WidenToDouble[Double, Double] = DoubleTop
    implicit def intToDouble: WidenToDouble[Int   , Double] = DoubleImpl

    private[Adjunct] object DoubleImpl extends DoubleTop with WidenToDouble[Int, Double] {
      final val id = 0x120

      def widen1(a: Int): Double = a.toDouble
    }
  }
  trait WidenToDouble[A1, A] extends Widen[A1, A] with NumDouble[A]

  trait NumDouble[A] extends NumFrac[A] {
    def sqrt      (a: A): A
    def exp       (a: A): A
    def midiCps   (a: A): A
    def cpsMidi   (a: A): A
    def midiRatio (a: A): A
    def ratioMidi (a: A): A
    def dbAmp     (a: A): A
    def ampDb     (a: A): A
    def octCps    (a: A): A
    def cpsOct    (a: A): A
    def log       (a: A): A
    def log2      (a: A): A
    def log10     (a: A): A
    def sin       (a: A): A
    def cos       (a: A): A
    def tan       (a: A): A
    def asin      (a: A): A
    def acos      (a: A): A
    def atan      (a: A): A
    def sinh      (a: A): A
    def cosh      (a: A): A
    def tanh      (a: A): A

    def atan2     (a: A, b: A): A
    def hypot     (a: A, b: A): A
    def hypotApx  (a: A, b: A): A
    def pow       (a: A, b: A): A

    def coin[Tx](a: A)(implicit r: Random[Tx], tx: Tx): Boolean
  }

  type ScalarNumDouble[A] = NumDouble[A] with Scalar[A]

  trait ToNumLowPriority {
    implicit def intSeqTop    : IntSeqTop   .type = IntSeqTop
    implicit def doubleSeqTop : DoubleSeqTop.type = DoubleSeqTop
  }
  object ToNum extends ToNumLowPriority {
    implicit def intTop       : IntTop      .type = IntTop
    implicit def doubleTop    : DoubleTop   .type = DoubleTop
    implicit def longTop      : LongTop     .type = LongTop
  }
  trait ToNum[A] extends Adjunct {
    type Int
    type Double
    type Long

    def toInt   (a: A): Int
    def toDouble(a: A): Double
    def toLong  (a: A): Long

    //    def int   : NumInt   [Int]
    //    def double: NumDouble[Double]
  }

  object FromAny {
//    val Unsupported: Try[Nothing] = Failure(new NoStackTrace {})

    // So this is all a bit nasty. Trying to remember why `IntTop` is not
    // an implicit object, but `BooleanTop` is. I think it has to do with
    // disambiguating `IntTop` and `IntSeqTop`.
    implicit def intTop     : IntTop    .type = IntTop
    implicit def doubleTop  : DoubleTop .type = DoubleTop
    implicit def longTop    : LongTop   .type = LongTop
//    implicit def booleanTop : BooleanTop.type = BooleanTop
//    implicit def stringTop  : StringTop .type = StringTop
    implicit def intSeqTop    : FromAny[Seq[Int     ]] = IntSeqTop
    implicit def doubleSeqTop : FromAny[Seq[Double  ]] = DoubleSeqTop
//    implicit def booleanSeqTop: FromAny[Seq[Boolean ]] = BooleanSeqTop

    def empty[A]: FromAny[A] = anyEmpty.asInstanceOf[FromAny[A]]

    private val anyEmpty = new Empty[Any]

    private final class Empty[A] extends FromAny[A] {
      def fromAny(in: Any): Option[A] = None

      def id: Int = throw new UnsupportedOperationException // XXX TODO --- do we may to store this instance?
    }
  }
  trait FromAny[A] extends Adjunct {
    /** Tries to extract a value of type `A` from an unknown input value.
      * If the input value is generally incompatible with `A`, returns `None`.
      *
      * The extraction should be direct and lossless. For example, a `FromAny[Int]`
      * should not try to parse a string, nor should it cast a `Long` to an `Int`.
      * On the other hand, a `FromAny[Double]` should accept a `Float` as input.
      */
    def fromAny(in: Any): Option[A]
  }

  object HasDefault {
    // So this is all a bit nasty. Trying to remember why `IntTop` is not
    // an implicit object, but `BooleanTop` is. I think it has to do with
    // disambiguating `IntTop` and `IntSeqTop`.
    implicit def intTop       : IntTop    .type = IntTop
    implicit def doubleTop    : DoubleTop .type = DoubleTop
    implicit def longTop      : LongTop   .type = LongTop
    implicit def intSeqTop    : HasDefault[Seq[Int]]    = IntSeqTop
    implicit def doubleSeqTop : HasDefault[Seq[Double]] = DoubleSeqTop
  }
  /** A type class saying some default value is provided for a type.
    * This is often a convention, such as "zero" for numeric types,
    * or "empty" for containers etc. It is used to avoid having to
    * initialize objects with explicit default values, but care must
    * be taken to take those conventional defaults into account.
    */
  trait HasDefault[A] extends Adjunct {
    def defaultValue: A
  }

  type ScalarToNum[A] = ToNum[A] with Scalar[A]

  trait Scalar[A] {
    final type In       = A

    final type Boolean  = scala.Boolean
    final type Int      = scala.Int
    final type Double   = scala.Double
    final type Long     = scala.Long
  }

  type ScalarEq[A] = Eq[A] with Scalar[A]

  object IntSeqTop
    extends NumInt      [Seq[Int]]
      with  SeqLikeNum  [Int]
      with  SeqLikeToNum[Int]
      with  FromAny     [Seq[Int]] {

    final val id = 1

    protected val peer: IntTop.type = IntTop

    def not(a: In): In = unOp(a)(peer.not)

    def and (a: In, b: In): In = binOp(a, b)(peer.and)
    def or  (a: In, b: In): In = binOp(a, b)(peer.or)
    def xor (a: In, b: In): In = binOp(a, b)(peer.xor)

    def lcm (a: In, b: In): In = binOp(a, b)(peer.lcm)
    def gcd (a: In, b: In): In = binOp(a, b)(peer.gcd)

    def shiftLeft         (a: In, b: In): In = binOp(a, b)(peer.shiftLeft)
    def shiftRight        (a: In, b: In): In = binOp(a, b)(peer.shiftRight)
    def unsignedShiftRight(a: In, b: In): In = binOp(a, b)(peer.unsignedShiftRight)

    def div (a: In, b: In): In = binOp(a, b)(peer.div)

    def fromAny(in: Any): Option[Seq[scala.Int]] = in match {
      case sq: Seq[_] =>
        val b   = Seq.newBuilder[scala.Int]
        val it  = sq.iterator
        while (it.hasNext) {
          it.next() match {
            case i: scala.Int => b += i
            case _            => return None
          }
        }
        Some(b.result())

      case _ => None
    }
  }

  object IntTop
    extends NumInt          [Int]
      with  ScalarEqImpl    [Int]
      with  ScalarToNumImpl [Int]
      with  FromAny         [Int]
      with  HasDefault      [Int] {

    final val id = 0

    def zero   : Int = 0
    def one    : Int = 1

    def toInt     (a: Int): Int     = a
    def toDouble  (a: Int): Double  = a.toDouble
    def toLong    (a: Int): Long    = a.toLong

    def plus      (a: Int, b: Int): Int = a + b
    def minus     (a: Int, b: Int): Int = a - b
    def times     (a: Int, b: Int): Int = a * b
    def rem       (a: Int, b: Int): Int = a % b
    def mod       (a: Int, b: Int): Int = ri.mod(a, b)
    def min       (a: Int, b: Int): Int = ri.min(a, b)
    def max       (a: Int, b: Int): Int = ri.max(a, b)

    def and(a: Int, b: Int): Int = a & b
    def or(a: Int, b: Int): Int = a | b
    def xor(a: Int, b: Int): Int = a ^ b
    def lcm       (a: Int, b: Int): Int = ri.lcm(a, b)
    def gcd       (a: Int, b: Int): Int = ri.gcd(a, b)

    def div       (a: Int, b: Int): Int = a / b

    def roundTo   (a: Int, b: Int): Int = ri2.roundTo  (a, b)
    def roundUpTo (a: Int, b: Int): Int = ri2.roundUpTo(a, b)
    def trunc     (a: Int, b: Int): Int = ri2.trunc    (a, b)

    def shiftLeft         (a: Int, b: Int): Int = a << b
    def shiftRight        (a: Int, b: Int): Int = a >> b
    def unsignedShiftRight(a: Int, b: Int): Int = a >>> b

    def difSqr    (a: Int, b: Int): Int = ri2.difSqr(a, b).toInt
    def sumSqr    (a: Int, b: Int): Int = ri2.sumSqr(a, b).toInt
    def sqrSum    (a: Int, b: Int): Int = ri2.sqrSum(a, b).toInt
    def sqrDif    (a: Int, b: Int): Int = ri2.sqrDif(a, b).toInt
    def absDif    (a: Int, b: Int): Int = ri2.absDif(a, b)

    def clip2     (a: Int, b: Int): Int = ri.clip2  (a, b)
    def excess    (a: Int, b: Int): Int = ri.excess (a, b)
    def fold2     (a: Int, b: Int): Int = ri.fold2  (a, b)
    def wrap2     (a: Int, b: Int): Int = ri.wrap2  (a, b)

    def negate    (a: Int): Int     = -a
    def abs       (a: Int): Int     = ri.abs(a)
    def signum    (a: Int): Int     = ri.signum(a)

    def not(a: Int): Int = ~a

    def squared   (a: Int): Int = ri.squared(a).toInt
    def cubed     (a: Int): Int = ri2.cubed (a).toInt

    def rand[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int = {
      val res = if (a >= 0) r.nextInt(a) // may throw exception
      else r.nextInt(-a) + a

      // assert(res >= 0 && res < a, s"a = $a, res = $res")

      res
    }

    def rand2[Tx](a: Int)(implicit r: Random[Tx], tx: Tx): Int = {
      val a1 = math.abs(a)
      r.nextInt(2 * a1 + 1) - a1
    }

    def rangeRand[Tx](a: Int, b: Int)(implicit r: Random[Tx], tx: Tx): Int =
      if (a < b) r.nextInt(b - a + 1) + a
      else       r.nextInt(a - b + 1) + b

    def lt  (a: Int, b: Int): Boolean = a <  b
    def lteq(a: Int, b: Int): Boolean = a <= b
    def gt  (a: Int, b: Int): Boolean = a >  b
    def gteq(a: Int, b: Int): Boolean = a >= b

    def fold(a: Int, lo: Int, hi: Int): Int = ri.fold(a, lo, hi)
    def clip(a: Int, lo: Int, hi: Int): Int = ri.clip(a, lo, hi)
    def wrap(a: Int, lo: Int, hi: Int): Int = ri.wrap(a, lo, hi)

    // ---- FromAny ----

    def fromAny(in: Any): Option[Int] = in match {
      case i: Int => Some(i)  // Success(i)
      case _      => None     // FromAny.Unsupported
    }

    // ---- HasDefault ----

    def defaultValue: Int = 0
  }

  object LongTop
    extends NumInt          [Long]
      with  ScalarEqImpl    [Long]
      with  ScalarToNumImpl [Long]
      with  FromAny         [Long]
      with  HasDefault      [Long] {

    final val id = 6

    def zero   : Long = 0L
    def one    : Long = 1L

    def toInt     (a: Long): Int    = a.toInt
    def toDouble  (a: Long): Double = a.toDouble
    def toLong    (a: Long): Long   = a

    def plus      (a: Long, b: Long): Long = a + b
    def minus     (a: Long, b: Long): Long = a - b
    def times     (a: Long, b: Long): Long = a * b
    def rem       (a: Long, b: Long): Long = a % b
    def mod       (a: Long, b: Long): Long = rl.mod(a, b)
    def min       (a: Long, b: Long): Long = rl.min(a, b)
    def max       (a: Long, b: Long): Long = rl.max(a, b)

    def and(a: Long, b: Long): Long = a & b
    def or(a: Long, b: Long): Long = a | b
    def xor(a: Long, b: Long): Long = a ^ b
    def lcm       (a: Long, b: Long): Long = rl.lcm(a, b)
    def gcd       (a: Long, b: Long): Long = rl.gcd(a, b)

    def div       (a: Long, b: Long): Long = a / b

    def roundTo   (a: Long, b: Long): Long = rl2.roundTo  (a, b)
    def roundUpTo (a: Long, b: Long): Long = rl2.roundUpTo(a, b)
    def trunc     (a: Long, b: Long): Long = rl2.trunc    (a, b)

    def shiftLeft         (a: Long, b: Long): Long = a << b
    def shiftRight        (a: Long, b: Long): Long = a >> b
    def unsignedShiftRight(a: Long, b: Long): Long = a >>> b

    def difSqr    (a: Long, b: Long): Long = rl2.difSqr(a, b)
    def sumSqr    (a: Long, b: Long): Long = rl2.sumSqr(a, b)
    def sqrSum    (a: Long, b: Long): Long = rl2.sqrSum(a, b)
    def sqrDif    (a: Long, b: Long): Long = rl2.sqrDif(a, b)
    def absDif    (a: Long, b: Long): Long = rl2.absDif(a, b)

    def clip2     (a: Long, b: Long): Long = rl.clip2  (a, b)
    def excess    (a: Long, b: Long): Long = rl.excess (a, b)
    def fold2     (a: Long, b: Long): Long = rl.fold2  (a, b)
    def wrap2     (a: Long, b: Long): Long = rl.wrap2  (a, b)

    def negate    (a: Long): Long     = -a
    def abs       (a: Long): Long     = rl.abs(a)
    def signum    (a: Long): Long     = rl.signum(a)

    def not(a: Long): Long = ~a

    def squared   (a: Long): Long = rl.squared(a)
    def cubed     (a: Long): Long = rl2.cubed (a)

    def rand[Tx](a: Long)(implicit r: Random[Tx], tx: Tx): Long = ???

    def rand2[Tx](a: Long)(implicit r: Random[Tx], tx: Tx): Long = ???

    def rangeRand[Tx](a: Long, b: Long)(implicit r: Random[Tx], tx: Tx): Long = ???

    def lt  (a: Long, b: Long): Boolean = a <  b
    def lteq(a: Long, b: Long): Boolean = a <= b
    def gt  (a: Long, b: Long): Boolean = a >  b
    def gteq(a: Long, b: Long): Boolean = a >= b

    def fold (a: Long, lo: Long, hi: Long): Long = rl.fold(a, lo, hi)
    def clip (a: Long, lo: Long, hi: Long): Long = rl.clip(a, lo, hi)
    def wrap (a: Long, lo: Long, hi: Long): Long = rl.wrap(a, lo, hi)

    // ---- FromAny ----

    def fromAny(in: Any): Option[Long] = in match {
      case n: Long  => Some(n)        //  Success(n)
      case i: Int   => Some(i.toLong) // Success(i.toLong)
      case _        => None           //  FromAny.Unsupported
    }

    // ---- HasDefault ----

    def defaultValue: Long = 0L
  }

  trait WidenSelfToDouble[A] extends WidenToDouble[A, A] {
    def widen1(a: A): A = a
  }

  object DoubleSeqTop
    extends SeqLikeNumFrac  [Double]
      with  SeqLikeToNum    [Double]
      with  SeqLikeNumDouble[Double]
      with  WidenSelfToDouble[Seq[Double]]
      with FromAny[Seq[Double]] {

    final val id = 3

    protected val peer: DoubleTop.type = DoubleTop

    def fromAny(in: Any): Option[Seq[scala.Double]] = in match {
      case sq: Seq[_] =>
        val b   = Seq.newBuilder[scala.Double]
        val it  = sq.iterator
        while (it.hasNext) {
          it.next() match {
            case d: scala.Double  => b += d
            case f: scala.Float   => b += f.toDouble
            case i: scala.Int     => b += i.toDouble
            case _                => return None
          }
        }
        Some(b.result())

      case _ => None
    }
  }

  object DoubleTop extends DoubleTop with WidenToDouble[Double, Double] {
    final val id = 2

    def widen1(a: Double): Double = a
  }

  sealed abstract class DoubleTop
    extends NumDouble       [Double]
      with  ScalarEqImpl    [Double]
      with  ScalarToNumImpl [Double]
      with  FromAny         [Double]
      with  HasDefault      [Double] {

    def zero   : In = 0.0
    def one    : In = 1.0

    // binary

    def plus      (a: In, b: In): In = rd.+(a, b)
    def minus     (a: In, b: In): In = rd.-(a, b)
    def times     (a: In, b: In): In = rd.*(a, b)
    def div       (a: In, b: In): In = rd./(a, b)
    def rem       (a: In, b: In): In = rd.%  (a, b)
    def mod       (a: In, b: In): In = rd.mod(a, b)

    def lt        (a: In, b: In): Boolean = a <  b
    def lteq      (a: In, b: In): Boolean = a <= b
    def gt        (a: In, b: In): Boolean = a >  b
    def gteq      (a: In, b: In): Boolean = a >= b

    def min       (a: In, b: In): In = rd.min(a, b)
    def max       (a: In, b: In): In = rd.max(a, b)

    def roundTo   (a: In, b: In): In = rd.roundTo(a, b)
    def roundUpTo (a: In, b: In): In = rd.roundUpTo(a, b)
    def trunc     (a: In, b: In): In = rd.trunc(a, b)

    def atan2     (a: In, b: In): In = rd.atan2   (a, b)
    def hypot     (a: In, b: In): In = rd.hypot   (a, b)
    def hypotApx  (a: In, b: In): In = rd.hypotApx(a, b)
    def pow       (a: In, b: In): In = rd.pow     (a, b)

    def difSqr(a: In, b: In): In = rd.difSqr(a, b)
    def sumSqr(a: In, b: In): In = rd.sumSqr(a, b)
    def sqrSum(a: In, b: In): In = rd.sqrSum(a, b)
    def sqrDif(a: In, b: In): In = rd.sqrDif(a, b)
    def absDif(a: In, b: In): In = rd.absDif(a, b)

    def clip2     (a: In, b: In): In = rd.clip2(a, b)
    def excess    (a: In, b: In): In = rd.excess(a, b)
    def fold2     (a: In, b: In): In = rd.fold2(a, b)
    def wrap2     (a: In, b: In): In = rd.wrap2(a, b)

    // unary

    def negate    (a: In): In = -a
    def abs       (a: In): In = rd.abs(a)
    def signum    (a: In): In = rd.signum(a)

    def toInt     (a: In): Int    = a.toInt
    def toDouble  (a: In): Double = a
    def toLong    (a: In): Long   = a.toLong

    def floor     (a: In): In     = rd.floor    (a)
    def ceil      (a: In): In     = rd.ceil     (a)
    def frac      (a: In): In     = rd.frac     (a)
    def midiCps   (a: In): In     = rd.midiCps  (a)
    def cpsMidi   (a: In): In     = rd.cpsMidi  (a)
    def midiRatio (a: In): In     = rd.midiRatio(a)
    def ratioMidi (a: In): In     = rd.ratioMidi(a)
    def dbAmp     (a: In): In     = rd.dbAmp    (a)
    def ampDb     (a: In): In     = rd.ampDb    (a)
    def octCps    (a: In): In     = rd.octCps   (a)
    def cpsOct    (a: In): In     = rd.cpsOct   (a)
    def log       (a: In): In     = rd.log      (a)
    def log2      (a: In): In     = rd.log2     (a)
    def log10     (a: In): In     = rd.log10    (a)
    def sin       (a: In): In     = rd.sin      (a)
    def cos       (a: In): In     = rd.cos      (a)
    def tan       (a: In): In     = rd.tan      (a)
    def asin      (a: In): In     = rd.asin     (a)
    def acos      (a: In): In     = rd.acos     (a)
    def atan      (a: In): In     = rd.atan     (a)
    def sinh      (a: In): In     = rd.sinh     (a)
    def cosh      (a: In): In     = rd.cosh     (a)
    def tanh      (a: In): In     = rd.tanh     (a)

    def sqrt(a: In): In = rd.sqrt(a)
    def exp (a: In): In = rd.exp (a)

    def squared (a: In): In = rd.squared(a)
    def cubed   (a: In): In = a * a * a

    def reciprocal(a: In): In = 1.0 / a

    def rand[Tx](a: In)(implicit r: Random[Tx], tx: Tx): In =
      r.nextDouble() * a

    def rand2[Tx](a: In)(implicit r: Random[Tx], tx: Tx): In =
      (r.nextDouble() * 2 - 1) * a

    def rangeRand[Tx](a: In, b: In)(implicit r: Random[Tx], tx: Tx): In =
      r.nextDouble() * (b - a) + a

    def coin[Tx](a: In)(implicit r: Random[Tx], tx: Tx): Boolean =
      r.nextDouble() < a

    def fold(a: In, lo: In, hi: In): In = rd.fold(a, lo, hi)
    def clip(a: In, lo: In, hi: In): In = rd.clip(a, lo, hi)
    def wrap(a: In, lo: In, hi: In): In = rd.wrap(a, lo, hi)

    // ---- FromAny ----

    def fromAny(in: Any): Option[Double] = in match {
      case d: Double  => Some(d)
      case f: Float   => Some(f.toDouble)
      case i: Int     => Some(i.toDouble)
      case _          => None
    }

    // ---- HasDefault ----

    def defaultValue: Double = 0.0
  }

  implicit object BooleanSeqTop
    extends NumBool[Seq[Boolean]]
      with SeqLikeEq    [Boolean]
      with SeqLikeToNum [Boolean]
      with FromAny[Seq[Boolean]] {

    val peer: BooleanTop.type = BooleanTop

    final val id = 5

    def negate(a: In): In = unOp(a)(!_)

    def and (a: In, b: In): In = binOp(a, b)(_ & _)
    def or  (a: In, b: In): In = binOp(a, b)(_ | _)
    def xor (a: In, b: In): In = binOp(a, b)(_ ^ _)

    def fromAny(in: Any): Option[Seq[scala.Boolean]] = in match {
      case sq: Seq[_] =>
        val b   = Seq.newBuilder[scala.Boolean]
        val it  = sq.iterator
        while (it.hasNext) {
          it.next() match {
            case x: scala.Boolean => b += x
            case _                => return None
          }
        }
        Some(b.result())

      case _ => None
    }
  }

  implicit object BooleanTop
    extends NumBool       [Boolean]
      with ScalarEqImpl   [Boolean]
      with ScalarToNumImpl[Boolean]
      with FromAny        [Boolean]
      with HasDefault     [Boolean] {

    final val id = 4

    def toInt   (a: In): Int    = if (a) 1    else 0
    def toDouble(a: In): Double = if (a) 1.0  else 0.0
    def toLong  (a: In): Long   = if (a) 1L   else 0L

    def negate(a: In): In = !a

    def and (a: In, b: In): In = a & b
    def or  (a: In, b: In): In = a | b
    def xor (a: In, b: In): In = a ^ b

    // ---- FromAny ----

    def fromAny(in: Any): Option[Boolean] = in match {
      case b: Boolean => Some(b)
      case _          => None
    }

    // ---- HasDefault ----

    def defaultValue: Boolean = false
  }

  implicit object StringTop
    extends ScalarEqImpl[String]
    with    FromAny     [String]
    with    HasDefault  [String] {

    final val id = 10

    // ---- FromAny ----

    def fromAny(in: Any): Option[String] = in match {
      case s: String  => Some(s)
      case _          => None
    }

    // ---- HasDefault ----

    def defaultValue: String = ""
  }

  // ---- extensibility ----

  trait Factory {
    def id: Int

    def readIdentifiedAdjunct(in: DataInput): Adjunct
  }

  private final val sync = new AnyRef

  @volatile private var factoryMap = Map.empty[Int, Factory]

  def addFactory(f: Factory): Unit = {
    val adjId = f.id
    if (adjId < 1000) throw new IllegalArgumentException(s"Third party adjunct id ($adjId) must be >= 1000")
    sync.synchronized {
      if (factoryMap.contains(adjId))
        throw new IllegalArgumentException(s"Adjunct $adjId was already registered ($f overrides ${factoryMap(adjId)})")

      factoryMap += adjId -> f
    }
  }

  @inline
  def getFactory(id: Int): Factory = factoryMap.getOrElse(id, sys.error(s"Unknown adjunct $id"))

  sealed trait Primitive extends Adjunct

  trait Context[T <: Exec[T]] {
    //  /** Creates a new pseudo-random number generator. */
    //  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): TxnRandom[S]
  }
}
/* sealed */ trait Adjunct extends Writable {
  def id: Int

  def write(out: DataOutput): Unit = {
    out.writeShort(Adjunct.COOKIE)
    out.writeShort(id)
  }
}