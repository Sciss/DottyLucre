/*
 *  UnaryOp.scala
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
package graph

import de.sciss.file._
import de.sciss.lucre.Adjunct.{HasDefault, Num, NumBool, NumFrac, NumInt, ScalarOrd, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.expr.graph.UnaryOp.Op
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.impl.IEventImpl
import de.sciss.lucre.{Adjunct, Exec, IExpr, ITargets, ProductWithAdjuncts, Txn}
import de.sciss.model.Change
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

object UnaryOp {
  abstract class Op[A1, A2] extends Product {
    def apply(a: A1): A2
  }

  abstract class NamedOp[A1, A2] extends Op[A1, A2] {
    override def productPrefix = s"UnaryOp$$$name"

    override def toString: String = name

    def name: String
  }
  
  type Adjuncts = scala.List[Adjunct]

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.negate(a)

    override def name = "Neg"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.negate(a)

    override def name = "Not"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class BitNot[A]()(implicit num: NumInt[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.not(a)

    override def name = "BitNot"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.abs(a)

    override def name = "Abs"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toDouble(a)

    override def name = "ToDouble"

    override def adjuncts: Adjuncts = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toInt(a)

    override def name = "ToInt"

    override def adjuncts: Adjuncts = to :: Nil
  }

  final case class ToLong[A, B]()(implicit to: ToNum[A] { type Long = B }) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = to.toLong(a)

    override def name = "ToLong"

    override def adjuncts: Adjuncts = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.ceil(a)

    override def name = "Ceil"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.floor(a)

    override def name = "Floor"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.frac(a)

    override def name = "Frac"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.signum(a)

    override def name = "Signum"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.squared(a)

    override def name = "Squared"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAdjuncts {
    def apply(a: A): A = num.cubed(a)

    override def name = "Cubed"

    override def adjuncts: Adjuncts = num :: Nil
  }

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sqrt(wd.widen1(a))

    override def name = "Sqrt"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.exp(wd.widen1(a))

    override def name = "Exp"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = num.reciprocal(w.widen1(a))

    override def name = "Reciprocal"

    override def adjuncts: Adjuncts = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.midiCps(wd.widen1(a))

    override def name = "Midicps"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cpsMidi(wd.widen1(a))

    override def name = "Cpsmidi"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.midiRatio(wd.widen1(a))

    override def name = "Midiratio"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.ratioMidi(wd.widen1(a))

    override def name = "Ratiomidi"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.dbAmp(wd.widen1(a))

    override def name = "Dbamp"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.ampDb(wd.widen1(a))

    override def name = "Ampdb"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.octCps(wd.widen1(a))

    override def name = "Octcps"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cpsOct(wd.widen1(a))

    override def name = "Cpsoct"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log(wd.widen1(a))

    override def name = "Log"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log2(wd.widen1(a))

    override def name = "Log2"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.log10(wd.widen1(a))

    override def name = "Log10"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sin(wd.widen1(a))

    override def name = "Sin"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cos(wd.widen1(a))

    override def name = "Cos"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.tan(wd.widen1(a))

    override def name = "Tan"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.asin(wd.widen1(a))

    override def name = "Asin"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.acos(wd.widen1(a))

    override def name = "Acos"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.atan(wd.widen1(a))

    override def name = "Atan"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.sinh(wd.widen1(a))

    override def name = "Sinh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.cosh(wd.widen1(a))

    override def name = "Cosh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAdjuncts {
    def apply(a: A): B = wd.tanh(wd.widen1(a))

    override def name = "Tanh"

    override def adjuncts: Adjuncts = wd :: Nil
  }

//  final case class Rand[A]()(implicit num: Num[A])

//  final case class Rand2

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

//  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B })

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve

  // ---- general ----

  final case class ToStr[A]() extends NamedOp[A, String] {
    def apply(a: A) : String = a.toString

    override def name = "ToStr"
  }

  // ---- Option ----

  final case class OptionSome[A]() extends NamedOp[A, Option[A]] {
    def apply(a: A): Option[A] = Some(a)

    override def name = "OptionSome"
  }

  final case class OptionIsEmpty[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isEmpty

    override def name = "OptionIsEmpty"
  }

  final case class OptionIsDefined[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isDefined

    override def name = "OptionIsDefined"
  }

  final case class OptionToList[A]() extends NamedOp[Option[A], scala.List[A]] {
    def apply(a: Option[A]): scala.List[A] = a.toList

    override def name = "OptionToList"
  }

 // XXX TODO are we sure that this is the way to go
 // and that we do not want to add an exception throwing mechanism to Ex?
  final case class OptionGet[A]()(implicit d: HasDefault[A])
    extends NamedOp[Option[A], A] with ProductWithAdjuncts {

    def apply(a: Option[A]): A = a.getOrElse(d.defaultValue)

    override def name = "OptionGet"

    override def adjuncts: Adjuncts = d :: Nil
  }

  // ---- Tuple2 ----

  final case class Tuple2_1[A, B]() extends NamedOp[(A, B), A] {
    def apply(a: (A, B)): A = a._1

    override def name = "Tuple2_1"
  }

  final case class Tuple2_2[A, B]() extends NamedOp[(A, B), B] {
    def apply(a: (A, B)): B = a._2

    override def name = "Tuple2_2"
  }

  final case class Tuple2Swap[A, B]() extends NamedOp[(A, B), (B, A)] {
    def apply(a: (A, B)): (B, A) = a.swap

    override def name = "Tuple2Swap"
  }

  // ---- Seq ----

  final case class SeqDistinct[A]() extends NamedOp[Seq[A], Seq[A]] {
    def apply(a: Seq[A]): Seq[A] = a.distinct

    override def name = "SeqDistinct"
  }

  final case class SeqHeadOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.headOption

    override def name = "SeqHeadOption"
  }

  final case class SeqIndices[A]() extends NamedOp[Seq[A], Seq[Int]] {
    def apply(a: Seq[A]): Seq[Int] = a.indices

    override def name = "SeqIndices"
  }

  final case class SeqIsEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.isEmpty

    override def name = "SeqIsEmpty"
  }

  final case class SeqLastOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.lastOption

    override def name = "SeqLastOption"
  }

  final case class SeqMaxOption[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Option[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Option[A] = if (a.isEmpty) None else Some(a.max(Ordering.fromLessThan[A](ord.lt)))

    override def name = "SeqMaxOption"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  final case class SeqMinOption[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Option[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Option[A] = if (a.isEmpty) None else Some(a.min(Ordering.fromLessThan[A](ord.lt)))

    override def name = "SeqMinOption"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  final case class SeqNonEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.nonEmpty

    override def name = "SeqNonEmpty"
  }

  final case class SeqPermutations[A]() extends NamedOp[Seq[A], Seq[Seq[A]]] {
    def apply(a: Seq[A]): Seq[Seq[A]] = a.permutations.toIndexedSeq

    override def name = "SeqPermutations"
  }

  final case class SeqProduct[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], A] with ProductWithAdjuncts {
    def apply(a: Seq[A]): A = a.foldLeft(num.one)(num.times)

    override def name = "SeqProduct"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class SeqReverse[A]() extends NamedOp[Seq[A], Seq[A]] {
    def apply(a: Seq[A]): Seq[A] = a.reverse

    override def name = "SeqReverse"
  }

//  final case class SeqSelect[A]()(implicit bridge: Obj.Bridge[A])
//    extends NamedOp[Seq[Obj], Seq[A]] with ProductWithAdjuncts {
//
//    def apply(a: Seq[Obj]): Seq[A] = a.flatMap(_.peer.flatMap(bridge.tryParseObj))
//
//    override def name = "SeqSelect"
//
//    def adjuncts: List[Adjunct] = from :: Nil
//  }
//
//  final case class SeqSelectFirst[A]()(implicit from: FromAny[A])
//    extends NamedOp[Seq[Obj], Option[A]] with ProductWithAdjuncts {
//
//    def apply(a: Seq[Obj]): Option[A] = {
//      val it = a.iterator.flatMap(from.fromAny)
//      if (it.hasNext) Some(it.next()) else None
//    }
//
//    override def name = "SeqSelectFirst"
//
//    def adjuncts: List[Adjunct] = from :: Nil
//  }

  final case class SeqSize[A]() extends NamedOp[Seq[A], Int] {
    def apply(a: Seq[A]): Int = a.size

    override def name = "SeqSize"
  }

  final case class SeqSorted[A]()(implicit ord: ScalarOrd[A])
    extends NamedOp[Seq[A], Seq[A]] with ProductWithAdjuncts {

    def apply(a: Seq[A]): Seq[A] = a.sorted(Ordering.fromLessThan(ord.lt))

    override def name = "SeqSorted"

    def adjuncts: List[Adjunct] = ord :: Nil
  }

  final case class SeqSum[A]()(implicit num: Num[A]) extends NamedOp[Seq[A], A] with ProductWithAdjuncts {
    def apply(a: Seq[A]): A = a.foldLeft(num.zero)(num.plus)

    override def name = "SeqSum"

    def adjuncts: List[Adjunct] = num :: Nil
  }

  final case class SeqZipWithIndex[A]() extends NamedOp[Seq[A], Seq[(A, Int)]] {
    def apply(a: Seq[A]): Seq[(A, Int)] = a.zipWithIndex

    override def name = "SeqZipWithIndex"
  }

  // ---- String ----

  final case class StringIsEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = a.isEmpty

    override def name = "StringIsEmpty"
  }

  final case class StringNonEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = !a.isEmpty
    override def name = "StringNonEmpty"
  }

  final case class StringLength() extends NamedOp[String, Int] {
    def apply(a: String): Int = a.length

    override def name = "StringLength"
  }

  final case class StringToIntOption() extends NamedOp[String, Option[Int]] {
    def apply(a: String): Option[Int] = {
      // Scala 2.13+ only: a.toIntOption
      try {
        val v = java.lang.Integer.parseInt(a)
        Some(v)
      } catch {
        case _: NumberFormatException => None
      }
    }

    override def name = "StringToIntOption"
  }

  final case class StringToDoubleOption() extends NamedOp[String, Option[Double]] {
    def apply(a: String): Option[Double] = {
      // Scala 2.13+ only: a.toDoubleOption
      try {
        val v = java.lang.Double.parseDouble(a)
        Some(v)
      } catch {
        case _: NumberFormatException => None
      }
    }

    override def name = "StringToDoubleOption"
  }

  final case class StringToBooleanOption() extends NamedOp[String, Option[Boolean]] {
    def apply(a: String): Option[Boolean] = {
      // Scala 2.13+ only: a.toBooleanOption
      if      (a.equalsIgnoreCase("true"  )) Some(true  )
      else if (a.equalsIgnoreCase("false" )) Some(false )
      else None
    }

    override def name = "StringToBooleanOption"
  }

  // ---- SpanLike ----

  final case class SpanLikeIsEmpty() extends NamedOp[_SpanLike, Boolean] {
    def apply(a: _SpanLike): Boolean = a.isEmpty

    override def name = "SpanLikeIsEmpty"
  }

  final case class SpanLikeNonEmpty() extends NamedOp[_SpanLike, Boolean] {
    def apply(a: _SpanLike): Boolean = a.nonEmpty

    override def name = "SpanLikeNonEmpty"
  }

  final case class SpanLikeClosedOption() extends NamedOp[_SpanLike, Option[_Span]] {
    def apply(a: _SpanLike): Option[_Span] = a match {
      case sp: _Span  => Some(sp)
      case _          => None
    }

    override def name = "SpanLikeClosedOption"
  }

  final case class SpanLikeStartOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a.startOption

    override def name = "SpanLikeStartOption"
  }

  final case class SpanLikeStopOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a.stopOption

    override def name = "SpanLikeStopOption"
  }

  final case class SpanLikeLengthOption() extends NamedOp[_SpanLike, Option[Long]] {
    def apply(a: _SpanLike): Option[Long] = a match {
      case sp: _Span.SpanOrVoid => Some(sp.length)
      case _                    => None
    }

    override def name = "SpanLikeLengthOption"
  }

  // ---- Span ----

  final case class SpanStart() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.start

    override def name = "SpanStart"
  }

  final case class SpanStop() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.stop

    override def name = "SpanStop"
  }

  final case class SpanLength() extends NamedOp[_Span, Long] {
    def apply(a: _Span): Long = a.length

    override def name = "SpanLength"
  }

  // ---- File ----

  final case class FileParentOption() extends NamedOp[File, Option[File]] {
    def apply(a: File): Option[File] = a.parentOption

    def name = "FileParentOption"
  }

  final case class FilePath() extends NamedOp[File, String] {
    def apply(a: File): String = a.path

    def name = "FilePath"
  }

  final case class FileName() extends NamedOp[File, String] {
    def apply(a: File): String = a.name

    def name = "FileName"
  }

  final case class FileBase() extends NamedOp[File, String] {
    def apply(a: File): String = a.base

    def name = "FileBase"
  }

  final case class FileExtL() extends NamedOp[File, String] {
    def apply(a: File): String = a.extL

    def name = "FileExtL"
  }

  // ---- Impl ----

  private[lucre] final class Expanded[T <: Exec[T], A1, A](op: Op[A1, A], a: IExpr[T, A1], tx0: T)
                                                          (implicit targets: ITargets[T])
    extends MappedIExpr[T, A1, A](a, tx0) with IEventImpl[T, Change[A]] {

    override def toString: String = s"UnaryOp($op, $a)"

    protected def mapValue(av: A1)(implicit tx: T): A = op(av)
  }

  // XXX TODO: let's do this at another point when `Const` is no longer `Lazy`
//  def apply1[A1, A](op: Op[A1, A], a: Ex[A1]): Ex[A] = a match {
//    case Const(av)  => Const(op.apply(av))
//    case _          => UnaryOp[A1, A](op, a)
//  }
}

final case class UnaryOp[A1, A](op: Op[A1, A], a: Ex[A1])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    new UnaryOp.Expanded[T, A1, A](op, ax, tx)
  }
}
