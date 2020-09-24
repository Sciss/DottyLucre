/*
 *  BinaryOp.scala
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
package graph

import de.sciss.file._
import de.sciss.lucre.Adjunct.{HasDefault, Num, NumDouble, NumFrac, NumInt, NumLogic, Ord, Widen2}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, IChangeEvent, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn}
import de.sciss.span.SpanLike

object BinaryOp {
  abstract class Op[A, B, C] extends Product {
    def apply(a: A, b: B): C
  }
  
  abstract class NamedOp[A, B, C] extends Op[A, B, C] {
    override def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }
  
  type Adjuncts = scala.List[Adjunct]

  // ---- (Num, Num) -> Num ----

  final case class Plus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.plus(widen.widen1(a), widen.widen2(b))

    def name = "Plus"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Minus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.minus(widen.widen1(a), widen.widen2(b))

    def name = "Minus"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Times[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.times(widen.widen1(a), widen.widen2(b))

    def name = "Times"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumFrac[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.div(widen.widen1(a), widen.widen2(b))

    def name = "Div"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class ModJ[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.rem(widen.widen1(a), widen.widen2(b))

    def name = "ModJ"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Mod[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.mod(widen.widen1(a), widen.widen2(b))

    def name = "Mod"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = eq.eq(a, b)

    def name = "Eq"

    override def adjuncts: Adjuncts = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Adjunct.Eq[A] { type Boolean = B})
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = eq.neq(a, b)

    def name = "Neq"

    override def adjuncts: Adjuncts = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.lt(a, b)

    def name = "Lt"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.gt(a, b)

    def name = "Gt"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.lteq(a, b)

    def name = "Leq"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAdjuncts {

    def apply(a: A, b: A): B = ord.gteq(a, b)

    def name = "Geq"

    override def adjuncts: Adjuncts = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.min(widen.widen1(a), widen.widen2(b))

    def name = "Min"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Max[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.max(widen.widen1(a), widen.widen2(b))

    def name = "Max"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class And[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.and(a, b)

    def name = "And"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Or[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.or(a, b)

    def name = "Or"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Xor[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.xor(a, b)

    def name = "Xor"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class IDiv[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.div(a, b)

    def name = "IDiv"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.lcm(a, b)

    def name = "Lcm"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.gcd(a, b)

    def name = "Gcd"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class RoundTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.roundTo(widen.widen1(a), widen.widen2(b))

    def name = "RoundTo"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class RoundUpTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.roundUpTo(widen.widen1(a), widen.widen2(b))

    def name = "RoundUpTo"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Trunc[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.trunc(widen.widen1(a), widen.widen2(b))

    def name = "Trunc"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Atan2[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.atan2(widen.widen1(a), widen.widen2(b))

    def name = "Atan2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Hypot[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.hypot(widen.widen1(a), widen.widen2(b))

    def name = "Hypot"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Hypotx[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.hypotApx(widen.widen1(a), widen.widen2(b))

    def name = "Hypotx"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Pow[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.pow(widen.widen1(a), widen.widen2(b))

    def name = "Pow"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.shiftLeft(a, b)

    def name = "LeftShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.shiftRight(a, b)

    def name = "RightShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAdjuncts {

    def apply(a: A, b: A): A = num.unsignedShiftRight(a, b)

    def name = "UnsignedRightShift"

    override def adjuncts: Adjuncts = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.difSqr(widen.widen1(a), widen.widen2(b))

    def name = "Difsqr"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Sumsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sumSqr(widen.widen1(a), widen.widen2(b))

    def name = "Sumsqr"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Sqrsum[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sqrSum(widen.widen1(a), widen.widen2(b))

    def name = "Sqrsum"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Sqrdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.sqrDif(widen.widen1(a), widen.widen2(b))

    def name = "Sqrdif"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Absdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.absDif(widen.widen1(a), widen.widen2(b))

    def name = "Absdif"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  final case class Clip2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.clip2(widen.widen1(a), widen.widen2(b))

    def name = "Clip2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Excess[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.excess(widen.widen1(a), widen.widen2(b))

    def name = "Excess"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Fold2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.fold2(widen.widen1(a), widen.widen2(b))

    def name = "Fold2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Wrap2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B): C = num.wrap2(widen.widen1(a), widen.widen2(b))

    def name = "Wrap2"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  // ---- Option ----

  final case class OptionContains[A]() extends NamedOp[Option[A], A, Boolean] {
    def apply(a: Option[A], b: A): Boolean = a.contains(b)
    
    def name = "OptionContains"
  }

  final case class OptionGetOrElse[A]() extends NamedOp[Option[A], A, A] {
    def apply(a: Option[A], b: A): A = a.getOrElse(b)
    
    def name = "OptionGetOrElse"
  }

  final case class OptionOrElse[A]() extends NamedOp[Option[A], Option[A], Option[A]] {
    def apply(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    def name = "OptionGetOrElse"
  }

  // ---- Seq ----

  final case class SeqAppended[A, B >: A]() extends NamedOp[Seq[A], B, Seq[B]] {
    def apply(a: Seq[A], b: B): Seq[B] = a :+ b

    def name = "SeqAppended"
  }

  final case class SeqApply[A]()(implicit d: HasDefault[A])
    extends NamedOp[Seq[A], Int, A] with ProductWithAdjuncts {

    def apply(a: Seq[A], b: Int): A = if (b < 0 || a.lengthCompare(b) <= 0) {
      d.defaultValue
    } else {
      a.apply(b)
    }

    def name = "SeqApply"

    override def adjuncts: Adjuncts = d :: Nil
  }

  final case class SeqApplyOption[A]() extends NamedOp[Seq[A], Int, Option[A]] {
    def apply(a: Seq[A], b: Int): Option[A] = if (b < 0) None else {
      if (a.lengthCompare(b) <= 0) None else Some(a.apply(b))
    }

    def name = "SeqApplyOption"
  }

  final case class SeqConcat[A]() extends NamedOp[Seq[A], Seq[A], Seq[A]] {
    def apply(a: Seq[A], b: Seq[A]): Seq[A] = a ++ b

    def name = "SeqConcat"
  }

  final case class SeqContains[A, B >: A]() extends NamedOp[Seq[A], B, Boolean] {
    def apply(a: Seq[A], b: B): Boolean = a.contains(b)

    def name = "SeqContains"
  }

  final case class SeqDiff[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Seq[A]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[A] = a.diff(b)

    def name = "SeqDiff"
  }

  final case class SeqDrop[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.drop(b)

    def name = "SeqDrop"
  }

  final case class SeqDropRight[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.dropRight(b)

    def name = "SeqDropRight"
  }

  final case class SeqEndsWith[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Boolean] {
    def apply(a: Seq[A], b: Seq[B]): Boolean = a.endsWith(b)

    def name = "SeqEndsWith"
  }

  final case class SeqGrouped[A]() extends NamedOp[Seq[A], Int, Seq[Seq[A]]] {
    def apply(a: Seq[A], b: Int): Seq[Seq[A]] = a.grouped(b).toIndexedSeq

    def name = "SeqGrouped"
  }

  final case class SeqIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int] {
    def apply(a: Seq[A], b: B): Int = a.indexOf(b)

    def name = "SeqIndexOf"
  }

  final case class SeqIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int] {
    def apply(a: Seq[A], that: Seq[B]): Int = a.indexOfSlice(that)

    def name = "SeqIndexOfSlice"
  }

  final case class SeqIntersect[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Seq[A]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[A] = a.intersect(b)

    def name = "SeqIntersect"
  }

  final case class SeqIsDefinedAt[A]() extends NamedOp[Seq[A], Int, Boolean] {
    def apply(a: Seq[A], b: Int): Boolean = a.isDefinedAt(b)

    def name = "SeqIsDefinedAt"
  }

  final case class SeqLastIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int] {
    def apply(a: Seq[A], b: B): Int = a.lastIndexOf(b)

    def name = "SeqLastIndexOf"
  }

  final case class SeqLastIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int] {
    def apply(a: Seq[A], that: Seq[B]): Int = a.lastIndexOfSlice(that)

    def name = "SeqLastIndexOfSlice"
  }

  final case class SeqPrepended[A, B >: A]() extends NamedOp[Seq[A], B, Seq[B]] {
    def apply(a: Seq[A], b: B): Seq[B] = b +: a

    def name = "SeqPrepended"
  }

  final case class SeqSameElements[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Boolean] {
    def apply(a: Seq[A], b: Seq[B]): Boolean = a.sameElements(b)

    def name = "SeqSameElements"
  }

  final case class SeqSplitAt[A]() extends NamedOp[Seq[A], Int, (Seq[A], Seq[A])] {
    def apply(a: Seq[A], b: Int): (Seq[A], Seq[A]) = a.splitAt(b)

    def name = "SeqSplitAt"
  }

  final case class SeqTake[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.take(b)

    def name = "SeqTake"
  }

  final case class SeqTakeRight[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.takeRight(b)

    def name = "SeqTakeRight"
  }

  final case class SeqZip[A, B]() extends NamedOp[Seq[A], Seq[B], Seq[(A, B)]] {
    def apply(a: Seq[A], b: Seq[B]): Seq[(A, B)] = a zip b

    def name = "SeqZip"
  }

  // ---- String ----

  final case class StringConcat() extends NamedOp[String, String, String] {
    def apply(a: String, b: String): String = a + b

    def name = "StringConcat"
  }

  final case class StringContains() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.contains(b)

    def name = "StringContains"
  }

  final case class StringStartsWith() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.startsWith(b)

    def name = "StringStartsWith"
  }

  final case class StringEndsWith() extends NamedOp[String, String, Boolean] {
    def apply(a: String, b: String): Boolean = a.endsWith(b)

    def name = "StringEndsWith"
  }

  final case class StringIndexOf() extends NamedOp[String, String, Int] {
    def apply(a: String, b: String): Int = a.indexOf(b)

    def name = "StringIndexOf"
  }

  final case class StringLastIndexOf() extends NamedOp[String, String, Int] {
    def apply(a: String, b: String): Int = a.lastIndexOf(b)

    def name = "StringLastIndexOf"
  }

  final case class StringTake() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.take(b)

    def name = "StringTake"
  }

  final case class StringDrop() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.drop(b)

    def name = "StringDrop"
  }

  // ---- SpanLike ----

  final case class SpanLikeClip() extends NamedOp[SpanLike, Long, Long] {
    def apply(a: SpanLike, b: Long): Long = a.clip(b)

    def name = "SpanLikeClip"
  }

  final case class SpanLikeShift() extends NamedOp[SpanLike, Long, SpanLike] {
    def apply(a: SpanLike, b: Long): SpanLike = a.shift(b)

    def name = "SpanLikeShift"
  }

  final case class SpanLikeContains() extends NamedOp[SpanLike, Long, Boolean] {
    def apply(a: SpanLike, b: Long): Boolean = a.contains(b)

    def name = "SpanLikeContains"
  }

  final case class SpanLikeOverlaps() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.overlaps(b)

    def name = "SpanLikeOverlaps"
  }

  final case class SpanLikeTouches() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.touches(b)

    def name = "SpanLikeTouches"
  }

  final case class SpanLikeUnion() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.union(b)

    def name = "SpanLikeUnion"
  }

  final case class SpanLikeIntersect() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.intersect(b)

    def name = "SpanLikeIntersect"
  }

  // ---- File ----

  final case class FileReplaceExt() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceExt(s)

    def name = "FileReplaceExt"
  }

  final case class FileReplaceName() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceName(s)

    def name: String = "FileReplaceName"
  }

  final case class FileChild() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a./(s)

    def name: String = "FileChild"
  }

  // ---- Impl ----

  private[lucre] final class Expanded[T <: Exec[T], A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A],
                                                                   a: IExpr[T, A1], b: IExpr[T, A2], tx0: T)
                                                                  (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)

    override def toString: String = s"BinaryOp($op, $a, $b)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val aV  = pull.expr(a)
      val bV  = pull.expr(b)
      value1(aV, bV)
    }

    @inline
    private def value1(av: A1, bv: A2): A = {
      op.apply(av, bv)
    }

    def value(implicit tx: T): A = {
      val av = a.value
      val bv = b.value
      value1(av, bv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }
}
final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A], a: Ex[A1], b: Ex[A2])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    new BinaryOp.Expanded[T, A1, A2, A3, A](op, ax, bx, tx)
  }
}
