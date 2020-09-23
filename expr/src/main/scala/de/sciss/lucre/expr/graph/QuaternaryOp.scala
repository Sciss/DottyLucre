/*
 *  QuaternaryOp.scala
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

import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Adjunct, Exec, IChangeEvent, IExpr, IPull, ITargets, Txn}

object QuaternaryOp {
  abstract class Op[A, B, C, D, E] extends Product {
    def apply(a: A, b: B, c: C, d: D): E
  }

  abstract class NamedOp[A, B, C, D, E] extends Op[A, B, C, D, E] {
    override def productPrefix = s"QuaternaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- Seq ----

  final case class SeqMkString[A]() extends NamedOp[Seq[A], String, String, String, String] {
    def apply(a: Seq[A], b: String, c: String, d: String): String =
      a.mkString(b, c, d)

    def name = "SeqMkString"
  }

  final case class SeqPatch[A, B >: A]() extends NamedOp[Seq[A], Int, Seq[B], Int, Seq[B]] {
    def apply(a: Seq[A], from: Int, other: Seq[B], replaced: Int): Seq[B] =
      a.patch(from, other, replaced)

    def name = "SeqPatch"
  }

  private[lucre] final class Expanded[T <: Exec[T], A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                                       a: IExpr[T, A1], b: IExpr[T, A2],
                                                                       c: IExpr[T, A3], d: IExpr[T, A4],
                                                                       tx0: T)
                                                                  (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)
    d.changed.--->(this)(tx0)

    override def toString: String = s"QuaternaryOp($op, $a, $b, $c, $d)"

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val _1v = pull.expr(a)
      val _2v = pull.expr(b)
      val _3v = pull.expr(c)
      val _4v = pull.expr(d)

      value1(_1v, _2v, _3v, _4v)
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3, dv: A4): A = {
      op.apply(av, bv, cv, dv)
    }

    def value(implicit tx: T): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      val dv = d.value
      value1(av, bv, cv, dv)
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
      d.changed -/-> changed
    }
  }
}
final case class QuaternaryOp[A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                 a: Ex[A1], b: Ex[A2], c: Ex[A3], d: Ex[A4])
  extends Ex[A] {

  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    val cx = c.expand[T]
    val dx = d.expand[T]
    new QuaternaryOp.Expanded[T, A1, A2, A3, A4, A](op, ax, bx, cx, dx, tx)
  }
}
