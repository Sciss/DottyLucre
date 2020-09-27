/*
 *  ExOps.scala
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

import java.io.{File => _File}

import de.sciss.lucre.Adjunct.{Eq, HasDefault, Num, NumBool, NumDouble, NumFrac, NumInt, Ord, ScalarOrd, ToNum, Widen, Widen2, WidenToDouble}
import de.sciss.lucre.expr.graph.{Act, Attr, Changed, Ex, File, Latch, Obj, QuinaryOp => QuinOp, ToTrig, Trig, BinaryOp => BinOp, TernaryOp => TernOp, UnaryOp => UnOp, QuaternaryOp => QuadOp}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

// XXX TODO --- use constant optimizations
final class ExOps[A](private val x: Ex[A]) extends AnyVal {
  // unary element-wise

  def unary_-   (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Neg[A](), x)
  def unary_!   (implicit num: NumBool[A] ): Ex[A]          = UnOp(UnOp.Not[A](), x)
  def unary_~   (implicit num: NumInt[A]  ): Ex[A]          = UnOp(UnOp.BitNot[A](), x)
  def abs       (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Abs[A](), x)

  def toDouble  (implicit to: ToNum[A]): Ex[to.Double]      = UnOp(UnOp.ToDouble[A, to.Double]()(to), x)
  def toInt     (implicit to: ToNum[A]): Ex[to.Int]         = UnOp(UnOp.ToInt   [A, to.Int   ]()(to), x)
  def toLong    (implicit to: ToNum[A]): Ex[to.Long]        = UnOp(UnOp.ToLong  [A, to.Long  ]()(to), x)

  def ceil      (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Ceil    [A](), x)
  def floor     (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Floor   [A](), x)
  def frac      (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Frac    [A](), x)
  def signum    (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Signum  [A](), x)
  def squared   (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Squared [A](), x)
  def cubed     (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Cubed   [A](), x)

  def sqrt   [B](implicit wd: WidenToDouble[A, B]): Ex[B]   = UnOp(UnOp.Sqrt[A, B](), x)
  def exp    [B](implicit wd: WidenToDouble[A, B]): Ex[B]   = UnOp(UnOp.Exp [A, B](), x)

  def reciprocal[B](implicit w: Widen[A, B], num: NumFrac[B]): Ex[B] = UnOp(UnOp.Reciprocal[A, B](), x)

  def midiCps  [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Midicps   [A, B](), x)
  def cpsMidi  [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cpsmidi   [A, B](), x)
  def midiRatio[B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Midiratio [A, B](), x)
  def ratioMidi[B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Ratiomidi [A, B](), x)
  def dbAmp    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Dbamp     [A, B](), x)
  def ampDb    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Ampdb     [A, B](), x)

  def octCps   [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Octcps    [A, B](), x)
  def cpsOct   [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cpsoct    [A, B](), x)
  def log      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log       [A, B](), x)
  def log2     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log2      [A, B](), x)
  def log10    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log10     [A, B](), x)
  def sin      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Sin       [A, B](), x)
  def cos      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cos       [A, B](), x)
  def tan      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Tan       [A, B](), x)
  def asin     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Asin      [A, B](), x)
  def acos     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Acos      [A, B](), x)
  def atan     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Atan      [A, B](), x)
  def sinh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Sinh      [A, B](), x)
  def cosh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cosh      [A, B](), x)
  def tanh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Tanh      [A, B](), x)

  //  def rand      (implicit num: Num[A]       ): Ex[A]           = UnOp(UnOp.Rand  [A](), x)
  //  def rand2     (implicit num: Num[A]       ): Ex[A]           = UnOp(UnOp.Rand2 [A](), x)
  //  def coin      (implicit num: NumDouble[A] ): Ex[num.Boolean] = UnOp(UnOp.Coin  [A, num.Boolean]()(num), x)

  def toStr: Ex[String] = UnOp(UnOp.ToStr[A](), x)

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A])(implicit eq: Eq[A]): Ex[eq.Boolean] = BinOp(BinOp.Eq [A, eq.Boolean]()(eq), x, that)
  def sig_!= (that: Ex[A])(implicit eq: Eq[A]): Ex[eq.Boolean] = BinOp(BinOp.Neq[A, eq.Boolean]()(eq), x, that)

  def <  (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Lt [A, ord.Boolean]()(ord), x, that)
  def >  (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Gt [A, ord.Boolean]()(ord), x, that)
  def <= (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Leq[A, ord.Boolean]()(ord), x, that)
  def >= (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Geq[A, ord.Boolean]()(ord), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  /** Integer division */
  def /   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.IDiv[A](), x, that)

  /** Currently a shortcut for `&`. */
  def &&  (that: Ex[A])(implicit num: NumBool[A]): Ex[A] = BinOp(BinOp.And   [A](), x, that)
  /** Currently a shortcut for `|`. */
  def ||  (that: Ex[A])(implicit num: NumBool[A]): Ex[A] = BinOp(BinOp.Or    [A](), x, that)

  def lcm (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def <--- (attr: ExAttrLike[A]): Unit = ...
  //  def <--> (attr: ExAttrLike[A]): Unit = ...

  def linLin[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = {
    val op = QuinOp.LinLin[A, A1, A2]()
    QuinOp[A, A, A, A1, A1, A2](op, x, inLo, inHi, outLo, outHi)
  }

  def linExp[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] = {
    val op = QuinOp.LinExp[A, A1, A2]()
    QuinOp[A, A, A, A1, A1, A2](op, x, inLo, inHi, outLo, outHi)
  }

  def expLin[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] = {
    val op = QuinOp.ExpLin[A, A1, A2]()
    QuinOp[A, A, A, A1, A1, A2](op, x, inLo, inHi, outLo, outHi)
  }

  def expExp[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] = {
    val op = QuinOp.ExpExp[A, A1, A2]()
    QuinOp[A, A, A, A1, A1, A2](op, x, inLo, inHi, outLo, outHi)
  }

  //  def poll(label: Ex[String] = "poll", gate: Ex[Boolean] = true): Ex[A] =
  //    Poll(x, gate = gate, label = label)

  // ---- bridge to trigger ----

  def changed: Trig = Changed(x)

  /** Latches the expression based on the trigger argument.
   * The initial state of the returned expression corresponds to the
   * initial state of the input expression. Subsequent values are
   * updated and cached only when a trigger occurs.
   */
  def latch(tr: Trig): Ex[A] = Latch(x, tr)

  /** Alias for `latch` */
  def <| (tr: Trig): Ex[A] = Latch(x, tr)

  /** Views this expression as a (yet to make) `Obj`.
   * In order to actually create the object, `.make` has to be called on it.
   */
  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class ExBooleanOps(private val x: Ex[Boolean]) extends AnyVal {
  def toTrig: Trig = ToTrig(x)
}

final class ExStringOps(private val x: Ex[String]) extends AnyVal {
  def length  : Ex[Int]     = UnOp(UnOp.StringLength(), x)
  def size    : Ex[Int]     = length

  def isEmpty : Ex[Boolean] = UnOp(UnOp.StringIsEmpty(), x)
  def nonEmpty: Ex[Boolean] = UnOp(UnOp.StringNonEmpty(), x)

  def toIntOption     : Ex[Option[Int]]     = UnOp(UnOp.StringToIntOption     (), x)
  def toDoubleOption  : Ex[Option[Double]]  = UnOp(UnOp.StringToDoubleOption  (), x)
  def toBooleanOption : Ex[Option[Boolean]] = UnOp(UnOp.StringToBooleanOption (), x)

  def ++          (that: Ex[String]): Ex[String]  = BinOp(BinOp.StringConcat      (), x, that)
  def contains    (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringContains    (), x, that)
  def startsWith  (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringStartsWith  (), x, that)
  def endsWith    (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringEndsWith    (), x, that)
  def indexOf     (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringIndexOf     (), x, that)
  def lastIndexOf (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringLastIndexOf (), x, that)

  def take(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringTake(), x, n)
  def drop(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringDrop(), x, n)

  def slice(from: Ex[Int], until: Ex[Int]): Ex[String] = TernOp(TernOp.StringSlice(), x, from, until)

  /** Applies 'printf' style formatting. See `StringFormat` for details.
   */
  def format(args: Ex[Any]*): Ex[String] = graph.StringFormat(x, args)
}

// TODO:
// distinctBy, flatten, fold, foldLeft, foldRight, groupBy,
// groupMap, groupMapReduce, maxByOption, minByOption, partition, reduceLeftOption, reduceRightOption,
// scanLeft, scanRight, segmentLength, sortBy, sortWith, span, toMap, toSet, transpose, unzip
final class ExSeqOps[A](private val x: Ex[Seq[A]]) extends AnyVal {
  /** A concatenation of this sequence with `that` sequence */
  def ++ [B >: A](that: Ex[Seq[B]]): Ex[Seq[B]] = BinOp(BinOp.SeqConcat[B](), x, that)

  /** A new sequence with the element prepended */
  def +: [B >: A](elem: Ex[B]): Ex[Seq[B]] = BinOp(BinOp.SeqPrepended[A, B](), x, elem)

  /** A new sequence with the element appended */
  def :+ [B >: A](elem: Ex[B]): Ex[Seq[B]] = BinOp(BinOp.SeqAppended[A, B](), x, elem)

  /** A new sequence with the element appended */
  def appended[B >: A](elem: Ex[B]): Ex[Seq[B]] = BinOp(BinOp.SeqAppended[A, B](), x, elem)

  /** The element at a given `index` if the index is valid, otherwise the default value */
  def apply(index: Ex[Int])(implicit d: HasDefault[A]): Ex[A] = BinOp(BinOp.SeqApply[A](), x, index)

  /** The element at a given `index` if the index is valid */
  def applyOption(index: Ex[Int]): Ex[Option[A]] = BinOp(BinOp.SeqApplyOption[A](), x, index)

  /** A concatenation of this sequence with `that` sequence */
  def concat[B >: A](that: Ex[Seq[B]]): Ex[Seq[B]] = BinOp(BinOp.SeqConcat[B](), x, that)

  def count(p: Ex[A] => Ex[Boolean]): Ex[Int] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.Count(x, it, res)
  }

  /** Whether this collection contains an element or not */
  def contains(elem: Ex[A]): Ex[Boolean] = BinOp(BinOp.SeqContains[A, A](), x, elem)
  //  def contains[B >: A](elem: Ex[B]): Ex[Boolean] = BinOp(BinOp.SeqContains[A, B](), x, elem)

  /** The multiset difference between this sequence and `that` sequence */
  def diff(that: Ex[Seq[A]]): Ex[Seq[A]] = BinOp(BinOp.SeqDiff[A, A](), x, that)
  //  def diff[B >: A](that: Ex[Seq[B]]): Ex[Seq[A]] = BinOp(BinOp.SeqDiff[A, B](), x, that)

  /** All the elements of this sequence ignoring the duplicates */
  def distinct: Ex[Seq[A]] = UnOp(UnOp.SeqDistinct[A](), x)

  /** All elements except first `n` ones */
  def drop(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqDrop[A](), x, n)

  /** The rest of this sequence without its `n` last elements */
  def dropRight(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqDropRight[A](), x, n)

  def dropWhile(p: Ex[A] => Ex[Boolean]): Ex[Seq[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.DropWhile(x, it, res)
  }

  /** Tests whether this sequence ends with `that` sequence */
  def endsWith(that: Ex[Seq[A]]): Ex[Boolean] = BinOp(BinOp.SeqEndsWith[A, A](), x, that)
  //  def endsWith[B >: A](that: Ex[Seq[B]]): Ex[Boolean] = BinOp(BinOp.SeqEndsWith[A, B](), x, that)

  def exists(p: Ex[A] => Ex[Boolean]): Ex[Boolean] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.Exists(x, it, res)
  }

  def filter(p: Ex[A] => Ex[Boolean]): Ex[Seq[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.Filter(x, it, res)
  }

  def filterNot(p: Ex[A] => Ex[Boolean]): Ex[Seq[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.FilterNot(x, it, res)
  }

  def forall(p: Ex[A] => Ex[Boolean]): Ex[Boolean] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.Forall(x, it, res)
  }

  def find(p: Ex[A] => Ex[Boolean]): Ex[Option[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.Find(x, it, res)
  }

  def findLast(p: Ex[A] => Ex[Boolean]): Ex[Option[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.FindLast(x, it, res)
  }

  def flatMap[B, To](f: Ex[A] => B)(implicit fm: Ex.CanFlatMap[Seq, B, To]): To =
    fm.flatMap(x, f)

  /** Partitions elements in fixed size sequences */
  def grouped(size: Ex[Int]): Ex[Seq[Seq[A]]] = BinOp(BinOp.SeqGrouped[A](), x, size)

  /** The first element if the sequence is non-empty */
  def headOption: Ex[Option[A]] = UnOp(UnOp.SeqHeadOption[A](), x)

  /** The first element if the sequence is non-empty, otherwise the default value */
  def head(implicit d: HasDefault[A]): Ex[A] = headOption.get

  /** The index of the first occurrence of `elem` in this sequence, or `-1` if not found */
  def indexOf(elem: Ex[A]): Ex[Int] = BinOp(BinOp.SeqIndexOf[A, A](), x, elem)
  //  def indexOf[B >: A](elem: Ex[B]): Ex[Int] = BinOp(BinOp.SeqIndexOf[A, B](), x, elem)

  /** The index of the first occurrence of `elem` at or after `from` in this sequence, or `-1` if not found */
  def indexOf(elem: Ex[A], from: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqIndexOf[A, A](), x, elem, from)
  //  def indexOf[B >: A](elem: Ex[B], from: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqIndexOf[A, B](), x, elem, from)

  /** First index where this sequence contains `that` sequence as a slice, or `-1` if not found */
  def indexOfSlice(that: Ex[Seq[A]]): Ex[Int] = BinOp(BinOp.SeqIndexOfSlice[A, A](), x, that)
  //  def indexOfSlice[B >: A](that: Ex[Seq[B]]): Ex[Int] = BinOp(BinOp.SeqIndexOfSlice[A, B](), x, that)

  /** First index at or after `from` where this sequence contains `that` sequence as a slice, or `-1` if not found */
  def indexOfSlice(that: Ex[Seq[A]], from: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqIndexOfSlice[A, A](), x, that, from)
  //  def indexOfSlice[B >: A](that: Ex[Seq[B]], from: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqIndexOfSlice[A, B](), x, that, from)

  def indexWhere(p: Ex[A] => Ex[Boolean]): Ex[Int] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.IndexWhere(x, it, res)
  }

  /** Indices from zero until the size of this sequence */
  def indices: Ex[Seq[Int]] = UnOp(UnOp.SeqIndices[A](), x)

  /** The multiset intersection between this sequence and `that` sequence */
  def intersect(that: Ex[Seq[A]]): Ex[Seq[A]] = BinOp(BinOp.SeqIntersect[A, A](), x, that)
  //  def intersect[B >: A](that: Ex[Seq[B]]): Ex[Seq[A]] = BinOp(BinOp.SeqIntersect[A, B](), x, that)

  /** Whether an `index` lies within this sequence */
  def isDefinedAt(index: Ex[Int]): Ex[Boolean] = BinOp(BinOp.SeqIsDefinedAt[A](), x, index)

  def isEmpty: Ex[Boolean] = UnOp(UnOp.SeqIsEmpty  [A](), x)

  /** The index of the last occurrence of `elem` in this sequence, or `-1` if not found */
  def lastIndexOf(elem: Ex[A]): Ex[Int] = BinOp(BinOp.SeqLastIndexOf[A, A](), x, elem)
  //  def lastIndexOf[B >: A](elem: Ex[B]): Ex[Int] = BinOp(BinOp.SeqLastIndexOf[A, B](), x, elem)

  /** The index of the last occurrence of `elem` at or before `end` in this sequence, or `-1` if not found */
  def lastIndexOf(elem: Ex[A], end: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqLastIndexOf[A, A](), x, elem, end)
  //  def lastIndexOf[B >: A](elem: Ex[B], end: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqLastIndexOf[A, B](), x, elem, end)

  /** Last index where this sequence contains `that` sequence as a slice, or `-1` if not found */
  def lastIndexOfSlice(that: Ex[Seq[A]]): Ex[Int] = BinOp(BinOp.SeqLastIndexOfSlice[A, A](), x, that)
  //  def lastIndexOfSlice[B >: A](that: Ex[Seq[B]]): Ex[Int] = BinOp(BinOp.SeqLastIndexOfSlice[A, B](), x, that)

  /** Last index at or before `end` where this sequence contains `that` sequence as a slice, or `-1` if not found */
  def lastIndexOfSlice(that: Ex[Seq[A]], end: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqLastIndexOfSlice[A, A](), x, that, end)
  //  def lastIndexOfSlice[B >: A](that: Ex[Seq[B]], end: Ex[Int]): Ex[Int] = TernOp(TernOp.SeqLastIndexOfSlice[A, B](), x, that, end)

  /** The last element if the sequence is non-empty */
  def lastOption: Ex[Option[A]] = UnOp(UnOp.SeqLastOption[A](), x)

  /** The last element if the sequence is non-empty, otherwise the default value */
  def last(implicit d: HasDefault[A]): Ex[A] = lastOption.get

  def map[B, To](f: Ex[A] => B)(implicit m: Ex.CanMap[Seq, B, To]): To =
    m.map(x, f)

  def maxOption(implicit ord: ScalarOrd[A]): Ex[Option[A]] = UnOp(UnOp.SeqMaxOption[A](), x)

  def minOption(implicit ord: ScalarOrd[A]): Ex[Option[A]] = UnOp(UnOp.SeqMinOption[A](), x)

  def mkString: Ex[String] = mkString("")

  def mkString(sep: Ex[String]): Ex[String] = mkString("", sep, "")

  def mkString(start: Ex[String], sep: Ex[String], end: Ex[String]): Ex[String] =
    QuadOp(QuadOp.SeqMkString[A](), x, start, sep, end)

  def nonEmpty: Ex[Boolean] = UnOp(UnOp.SeqNonEmpty[A](), x)

  def padTo[B >: A](len: Ex[Int], elem: Ex[B]): Ex[Seq[B]] = TernOp(TernOp.SeqPadTo[A, B](), x, len, elem)

  def patch[B >: A](from: Ex[Int], other: Ex[Seq[B]], replaced: Ex[Int]): Ex[Seq[B]] =
    QuadOp(QuadOp.SeqPatch[A, B](), x, from, other, replaced)

  def permutations: Ex[Seq[Seq[A]]] = UnOp(UnOp.SeqPermutations[A](), x)

  def prepended[B >: A](elem: Ex[B]): Ex[Seq[B]] = BinOp(BinOp.SeqPrepended[A, B](), x, elem)

  def product(implicit num: Num[A]): Ex[A] = UnOp(UnOp.SeqProduct[A](), x)

  def reverse: Ex[Seq[A]] = UnOp(UnOp.SeqReverse[A](), x)

  def sameElements[B >: A](that: Ex[Seq[B]]): Ex[Boolean] = BinOp(BinOp.SeqSameElements[A, B](), x, that)

  def select[B](implicit bridge: Obj.Bridge[B], ev: Ex[Seq[A]] =:= Ex[Seq[Obj]]): Ex[Seq[B]] =
    ExSeq.Select(ev(x))

  def selectFirst[B](implicit bridge: Obj.Bridge[B], ev: Ex[Seq[A]] =:= Ex[Seq[Obj]]): Ex[Option[B]] =
    ExSeq.SelectFirst(ev(x))

  /** The number of elements in the sequence */
  def size: Ex[Int] = UnOp(UnOp.SeqSize[A](), x)

  def slice(from: Ex[Int], until: Ex[Int]): Ex[Seq[A]] = TernOp(TernOp.SeqSlice[A](), x, from, until)

  /** Groups elements in fixed size blocks by passing a "sliding window" over them.
   * Note that both `size` and `step` are automatically constraint to values of one and greater.
   */
  def sliding(size: Ex[Int], step: Ex[Int] = 1): Ex[Seq[Seq[A]]] = TernOp(TernOp.SeqSliding[A](), x, size, step)

  def sorted(implicit ord: ScalarOrd[A]): Ex[Seq[A]] = UnOp(UnOp.SeqSorted[A](), x)

  def splitAt(n: Ex[Int]): Ex[(Seq[A], Seq[A])] = BinOp(BinOp.SeqSplitAt[A](), x, n)

  /** Tests whether this sequence starts with `that` sequence */
  def startsWith(that: Ex[Seq[A]], offset: Ex[Int] = 0): Ex[Boolean] =
    TernOp(TernOp.SeqStartsWith[A, A](), x, that, offset)

  //  def startsWith[B >: A](that: Ex[Seq[B]], offset: Ex[Int] = 0): Ex[Boolean] = TernOp(TernOp.SeqStartsWith[A, B](), x, that, offset)

  def sum(implicit num: Num[A]): Ex[A] = UnOp(UnOp.SeqSum[A](), x)

  def take(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqTake[A](), x, n)

  def takeRight(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqTakeRight[A](), x, n)

  def takeWhile(p: Ex[A] => Ex[Boolean]): Ex[Seq[A]] = {
    val (it, res) = Ex.mkClosure(p)
    ExSeq.TakeWhile(x, it, res)
  }

  /** A new sequence equal to this sequence with one single replaced `elem` at `index`.
   * If the index lies outside the sequence, the original sequence is returned.
   */
  def updated[B >: A](index: Ex[Int], elem: Ex[B]): Ex[Seq[B]] = TernOp(TernOp.SeqUpdated[A, B](), x, index, elem)

  // used in for-comprehensions
  def withFilter(p: Ex[A] => Ex[Boolean]): Ex[Seq[A]] = filter(p)

  def zip[B](that: Ex[Seq[B]]): Ex[Seq[(A, B)]] = BinOp(BinOp.SeqZip[A, B](), x, that)

  def zipWithIndex: Ex[Seq[(A, Int)]] = UnOp(UnOp.SeqZipWithIndex[A](), x)
}

final class ExSpanOps[A <: _SpanLike](private val x: Ex[A]) extends AnyVal {
  def clip(pos: Ex[Long]): Ex[Long] = BinOp(BinOp.SpanLikeClip(), x, pos)

  def shift(delta: Ex[Long]): Ex[_SpanLike] = BinOp(BinOp.SpanLikeShift(), x, delta)

  def isEmpty : Ex[Boolean] = UnOp(UnOp.SpanLikeIsEmpty (), x)
  def nonEmpty: Ex[Boolean] = UnOp(UnOp.SpanLikeNonEmpty(), x)

  def contains(pos: Ex[Long]): Ex[Boolean] = BinOp(BinOp.SpanLikeContains(), x, pos)

  // cannot overload due to erasure
  //  def contains(that: Ex[SpanLike]): Ex[Boolean]

  def overlaps  (that: Ex[_SpanLike]): Ex[Boolean]    = BinOp(BinOp.SpanLikeOverlaps  (), x, that)
  def touches   (that: Ex[_SpanLike]): Ex[Boolean]    = BinOp(BinOp.SpanLikeTouches   (), x, that)

  def union     (that: Ex[_SpanLike]): Ex[_SpanLike]  = BinOp(BinOp.SpanLikeUnion     (), x, that)
  def intersect (that: Ex[_SpanLike]): Ex[_SpanLike]  = BinOp(BinOp.SpanLikeIntersect (), x, that)

  def closedOption: Ex[Option[_Span]] = UnOp(UnOp.SpanLikeClosedOption(), x)
  def startOption : Ex[Option[Long]]  = UnOp(UnOp.SpanLikeStartOption (), x)
  def stopOption  : Ex[Option[Long]]  = UnOp(UnOp.SpanLikeStopOption  (), x)
  def lengthOption: Ex[Option[Long]]  = UnOp(UnOp.SpanLikeLengthOption(), x)

  // ---- simplify to only span here ----

  def start (implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanStart (), ev(x))
  def stop  (implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanStop  (), ev(x))
  def length(implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanLength(), ev(x))
}

final class ExOptionOps[A](private val x: Ex[Option[A]]) extends AnyVal {
  def isEmpty   : Ex[Boolean] = UnOp(UnOp.OptionIsEmpty   [A](), x)
  def isDefined : Ex[Boolean] = UnOp(UnOp.OptionIsDefined [A](), x)
  def nonEmpty  : Ex[Boolean] = isDefined

  def get(implicit d: HasDefault[A]): Ex[A] = UnOp(UnOp.OptionGet(), x)

  def getOrElse [B >: A](default    : Ex[B])        : Ex[B]         = BinOp(BinOp.OptionGetOrElse[B](), x, default)
  def orElse    [B >: A](alternative: Ex[Option[B]]): Ex[Option[B]] = BinOp(BinOp.OptionOrElse   [B](), x, alternative)

  def contains[B >: A](elem: Ex[B]): Ex[Boolean] = BinOp(BinOp.OptionContains[B](), x, elem)

  def toList: Ex[scala.List[A]] = UnOp(UnOp.OptionToList[A](), x)

  def map[B, To](f: Ex[A] => B)(implicit m: Ex.CanMap[Option, B, To]): To =
    m.map(x, f)

  def flatMap[B, To](f: Ex[A] => B)(implicit fm: Ex.CanFlatMap[Option, B, To]): To =
    fm.flatMap(x, f)
}

final class IntLiteralExOps(private val x: Int) extends AnyVal {
  private type A = Int

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  //  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  //  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  //  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  //  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  //  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  //  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class LongLiteralExOps(private val x: Long) extends AnyVal {
  private type A = Long

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  //  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  //  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  //  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  //  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  //  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  //  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class DoubleLiteralExOps(private val x: Double) extends AnyVal {
  private type A = Double

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  //  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  //  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  //  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  //  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  //  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  //  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  //  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  //  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  //  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

// XXX TODO --- we could check here if arguments are constants,
// and in that case fall back to constant behaviour like `scala.StringOps`.
/** Some methods are here form `ExStringOps` again, so that we can
 * use them on plain string literals, without requiring an
 * explicit wrap such as `Const("x")` first.
 */
final class StringLiteralExOps(private val x: String) extends AnyVal {
  def attr[A](implicit bridge: Obj.Bridge[A]): Attr[A] = Attr(x)

  //  def attr[A]()(implicit bridge: Obj.Bridge[A], d: HasDefault[A]): Attr[A] = Attr(x)

  def attr[A](default: Ex[A])(implicit bridge: Obj.Bridge[A]): Attr.WithDefault[A] =
    Attr.WithDefault(x, default)

  def size: Int /*Ex[Int]*/ = x.length

  def nonEmpty: Boolean /*Ex[Boolean]*/ = !x.isEmpty

  def ++ (that: Ex[String]): Ex[String] = BinOp(BinOp.StringConcat(), x, that)

  def contains    (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringContains    (), x, that)
  def startsWith  (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringStartsWith  (), x, that)
  def endsWith    (that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringEndsWith    (), x, that)
  def indexOf     (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringIndexOf     (), x, that)
  def lastIndexOf (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringLastIndexOf (), x, that)

  def take(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringTake(), x, n)
  def drop(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringDrop(), x, n)

  def slice(from: Ex[Int], until: Ex[Int]): Ex[String] = TernOp(TernOp.StringSlice(), x, from, until)

  def format(args: Ex[Any]*): Ex[String] = graph.StringFormat(x, args)
}

final class ExTuple2Ops[A, B](private val x: Ex[(A, B)]) extends AnyVal {
  def _1: Ex[A] = UnOp(UnOp.Tuple2_1[A, B](), x)
  def _2: Ex[B] = UnOp(UnOp.Tuple2_2[A, B](), x)

  def swap: Ex[(B, A)] = UnOp(UnOp.Tuple2Swap[A, B](), x)
}

final class ExFileOps(private val x: Ex[_File]) extends AnyVal {
  // ---- expressions ----

  /** Returns the parent directory if it exists. */
  def parentOption: Ex[Option[_File]] =
    UnOp(UnOp.FileParentOption(), x)

  /** Returns the string representation of the file's path. */
  def path: Ex[String] =
    UnOp(UnOp.FilePath(), x)

  /** Returns the name part of the file. */
  def name: Ex[String] =
    UnOp(UnOp.FileName(), x)

  /** Returns the name part of the file and drops the extension (if any). */
  def base: Ex[String] =
    UnOp(UnOp.FileBase(), x)

  /** Returns the extension of the file (lower-cased, period dropped). Returns and empty string
   * if no extension is given.
   */
  def ext: Ex[String] =
    UnOp(UnOp.FileExtL(), x)  // ! simplify and use lower case here

  /** Replaces the extension part of this file. Parameter `s` may or may not contain a leading period. */
  def replaceExt(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceExt(), x, s)

  /** Replaces the name part of this file, keeping the parent directory. */
  def replaceName(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceName(), x, s)

  def / (child: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileChild(), x, child)

  // ---- actions ----

  /** Deletes the file */
  def delete: Act = File.Delete(x)

  /** Creates the directory and possibly parent directories denoted by this file. */
  def mkDir : Act = File.MkDir(x)

  // ---- hybrid ----

  /** Lists the contains of a directory */
  def list: Ex[Seq[_File]] with Act = File.List(x)
}
