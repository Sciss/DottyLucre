/*
 *  TernaryOp.scala
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

import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, IChangeEvent, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn}

object TernaryOp {
  abstract class Op[A, B, C, D] extends Product {
    def apply(a: A, b: B, c: C): D
  }
  
  abstract class NamedOp[A, B, C, D] extends Op[A, B, C, D] {
    override def productPrefix = s"TernaryOp$$$name"

    def name: String

    override def toString: String = name
  }
  
  type Adjuncts = scala.List[Adjunct]

  // ---- (Num, Num, Num) -> Num ----

  final case class Clip[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.clip(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Clip"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Fold[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.fold(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Fold"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  final case class Wrap[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, B, C] with ProductWithAdjuncts {

    def apply(a: A, b: B, c: B): C = num.wrap(widen.widen1(a), widen.widen2(b), widen.widen2(c))

    def name = "Wrap"

    override def adjuncts: Adjuncts = widen :: num :: Nil
  }

  // ---- String ----

  final case class StringSlice() extends NamedOp[String, Int, Int, String] {
    def apply(a: String, b: Int, c: Int): String = a.slice(b, c)

    def name = "StringSlice"
  }

  // ---- Seq ----

  final case class SeqIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int, Int] {
    def apply(a: Seq[A], elem: B, from: Int): Int = a.indexOf(elem, from)

    def name = "SeqIndexOf"
  }

  final case class SeqIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Int] {
    def apply(a: Seq[A], that: Seq[B], from: Int): Int = a.indexOfSlice(that, from)

    def name = "SeqIndexOfSlice"
  }

  final case class SeqLastIndexOf[A, B >: A]() extends NamedOp[Seq[A], B, Int, Int] {
    def apply(a: Seq[A], elem: B, from: Int): Int = a.lastIndexOf(elem, from)

    def name = "SeqLastIndexOf"
  }

  final case class SeqLastIndexOfSlice[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Int] {
    def apply(a: Seq[A], that: Seq[B], from: Int): Int = a.lastIndexOfSlice(that, from)

    def name = "SeqLastIndexOfSlice"
  }

  final case class SeqPadTo[A, B >: A]() extends NamedOp[Seq[A], Int, B, Seq[B]] {
    def apply(a: Seq[A], len: Int, elem: B): Seq[B] = a.padTo(len, elem)

    def name = "SeqPadTo"
  }

  final case class SeqSlice[A]() extends NamedOp[Seq[A], Int, Int, Seq[A]] {
    def apply(a: Seq[A], b: Int, c: Int): Seq[A] = a.slice(b, c)

    def name = "SeqSlice"
  }

  final case class SeqSliding[A]() extends NamedOp[Seq[A], Int, Int, Seq[Seq[A]]] {
    def apply(a: Seq[A], size: Int, step: Int): Seq[Seq[A]] =
      a.sliding(math.max(1, size), math.max(1, step)).toIndexedSeq

    def name = "SeqSliding"
  }

  final case class SeqStartsWith[A, B >: A]() extends NamedOp[Seq[A], Seq[B], Int, Boolean] {
    def apply(a: Seq[A], b: Seq[B], offset: Int): Boolean = a.startsWith(b, offset)

    def name = "SeqStartsWith"
  }

  final case class SeqUpdated[A, B >: A]() extends NamedOp[Seq[A], Int, B, Seq[B]] {
    def apply(a: Seq[A], index: Int, elem: B): Seq[B] =
      if (index >= 0 && index < a.size) a.updated(index, elem) else a

    def name = "SeqUpdated"
  }

  // ----

  private[lucre] final class Expanded[T <: Exec[T], A1, A2, A3, A](op: TernaryOp.Op[A1, A2, A3, A],
                                                                   a: IExpr[T, A1], b: IExpr[T, A2],
                                                                   c: IExpr[T, A3], tx0: T)
                                                                  (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)

    override def toString: String = s"TernaryOp($op, $a, $b, $c)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val _1v = pull.expr(a)
      val _2v = pull.expr(b)
      val _3v = pull.expr(c)

      value1(_1v, _2v, _3v)
    }

//    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] = {
//      val _1c = a.changed
//      val _2c = b.changed
//      val _3c = c.changed
//
//      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
//      val _2ch = if (pull.contains(_2c)) pull(_2c) else None
//      val _3ch = if (pull.contains(_3c)) pull(_3c) else None
//
//      (_1ch, _2ch, _3ch) match {
//        case (Some(ach), None, None) =>
//          val bv      = b.value
//          val cv      = c.value
//          val before  = value1(ach.before , bv, cv)
//          val now     = value1(ach.now    , bv, cv)
//          if (before == now) None else Some(Change(before, now))
//        case (None, Some(bch), None) =>
//          val av      = a.value
//          val cv      = c.value
//          val before  = value1(av, bch.before , cv)
//          val now     = value1(av, bch.now    , cv)
//          if (before == now) None else Some(Change(before, now))
//        case (None, None, Some(cch)) =>
//          val av      = a.value
//          val bv      = b.value
//          val before  = value1(av, bv, cch.before )
//          val now     = value1(av, bv, cch.now    )
//          if (before == now) None else Some(Change(before, now))
//        case (Some(ach), Some(bch), None) =>
//          val cv      = c.value
//          val before  = value1(ach.before , bch.before, cv)
//          val now     = value1(ach.now    , bch.now   , cv)
//          if (before == now) None else Some(Change(before, now))
//        case (None, Some(bch), Some(cch)) =>
//          val av      = a.value
//          val before  = value1(av, bch.before , cch.before)
//          val now     = value1(av, bch.now    , cch.now   )
//          if (before == now) None else Some(Change(before, now))
//        case (Some(ach), Some(bch), Some(cch)) =>
//          val before  = value1(ach.before , bch.before, cch.before)
//          val now     = value1(ach.now    , bch.now   , cch.now   )
//          if (before == now) None else Some(Change(before, now))
//        case _ => None
//      }
//    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3): A = {
      op.apply(av, bv, cv)
    }

    def value(implicit tx: T): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      value1(av, bv, cv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
    }
  }
}
final case class TernaryOp[A1, A2, A3, A](op: TernaryOp.Op[A1, A2, A3, A], a: Ex[A1], b: Ex[A2], c: Ex[A3])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    val cx = c.expand[T]
    new TernaryOp.Expanded[T, A1, A2, A3, A](op, ax, bx, cx, tx)
  }
}
