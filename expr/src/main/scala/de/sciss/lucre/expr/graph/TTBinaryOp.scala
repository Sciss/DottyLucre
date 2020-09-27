/*
 *  TTBinaryOp.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.{Context, ITrigger}
import de.sciss.lucre.impl.IEventImpl
import de.sciss.lucre.{Exec, IEvent, IPull, ITargets, Txn}

object TTBinaryOp {
  sealed trait Op extends Product {
    def apply(a: Boolean, b: Boolean): Boolean

    override final def productPrefix = s"TTBinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  final case class And() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a & b

    def name = "And"
  }

  final case class Or() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a | b

    def name = "Or"
  }

  final case class Xor() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a ^ b

    def name = "Xor"
  }

  private final class Expanded[T <: Exec[T]](op: Op, a: ITrigger[T], b: ITrigger[T], tx0: T)
                                            (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)

    override def toString: String = s"TTBinaryOp($op, $a, $b)"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      val _1c = a.changed
      val _2c = b.changed

      val at = pull.contains(_1c) && pull(_1c).isDefined
      val bt = pull.contains(_2c) && pull(_2c).isDefined

      val t = op(at, bt)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }
}
final case class TTBinaryOp(op: TTBinaryOp.Op, a: Trig, b: Trig) extends Trig {
  type Repr[T <: Txn[T]] = ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    new TTBinaryOp.Expanded[T](op, ax, bx, tx)
  }
}
