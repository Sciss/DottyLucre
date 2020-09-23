/*
 *  TBinaryOp.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.{Context, ITrigger}
import de.sciss.lucre.impl.IEventImpl
import de.sciss.lucre.{Exec, IEvent, IExpr, IPull, ITargets, Txn}

object TBinaryOp {
  sealed trait Op[A] extends Product {
    def apply(a: Boolean, b: A): Boolean

    override final def productPrefix = s"TBinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  final case class And() extends Op[Boolean] {
    def apply(a: Boolean, b: Boolean): Boolean = a & b

    def name = "And"
  }

  private final class Expanded[T <: Exec[T], A](op: Op[A], a: ITrigger[T], b: IExpr[T, A], tx0: T)
                                               (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    a.changed.--->(this)(tx0)

    override def toString: String = s"TBinaryOp($op, $a, $b)"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      val aEvt  = a.changed
      val at    = pull.contains(aEvt) && pull(aEvt).isDefined
      val bt    = b.value

      val t = op(at, bt)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: T): Unit =
      a.changed -/-> changed
  }
}
final case class TBinaryOp[A](op: TBinaryOp.Op[A], a: Trig, b: Ex[A]) extends Trig {
  type Repr[T <: Txn[T]] = ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    new TBinaryOp.Expanded[T, A](op, ax, bx, tx)
  }
}
