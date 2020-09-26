/*
 *  Latch.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.{Context, ITrigger, graph}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Caching, IChangeEvent, IExpr, IPull, IPush, ITargets, Txn}

import scala.concurrent.stm.Ref

object Latch {
  private final class Expanded[T <: Txn[T], A](in: IExpr[T, A], trig: ITrigger[T], tx0: T)
                                              (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] with Caching {

    override def toString: String = s"$in.latch($trig)"

    private[this] val ref = Ref(in.value(tx0))

    trig.changed.--->(this)(tx0)

    def value(implicit tx: T): A =
      IPush.tryPull(this).fold(ref())(_.now)

    def dispose()(implicit tx: T): Unit =
      trig.changed.-/->(this)

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
      if (phase.isBefore || pull(trig.changed).isEmpty) ref() else {
        val newValue  = in.value
        ref()         = newValue
        newValue
      }
  }
}
/** Latches the expression based on the trigger argument.
  * The initial state of the returned expression corresponds to the
  * initial state of the input expression. Subsequent values are
  * updated and cached only when a trigger occurs.
  */
final case class Latch[A](in: Ex[A], trig: Trig) extends Ex[A] {
  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new graph.Latch.Expanded(in.expand[T], trig.expand[T], tx)
  }
}
