/*
 *  ToTrig.scala
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
import de.sciss.lucre.{Exec, IEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

object ToTrig {
  private final class Expanded[T <: Exec[T]](in: IExpr[T, Boolean], tx0: T)
                                            (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    in.changed.--->(this)(tx0)

    override def toString: String = s"ToTrig($in)"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      val res: Option[Change[Boolean]] = pull(in.changed)
      val t = res.exists(ch => ch.isSignificant && ch.now)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: T): Unit =
      in.changed -/-> changed
  }
}
final case class ToTrig(in: Ex[Boolean]) extends Trig {
  type Repr[T <: Txn[T]] = ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val inExp = in.expand[T]
    new ToTrig.Expanded[T](inExp, tx)
  }
}
