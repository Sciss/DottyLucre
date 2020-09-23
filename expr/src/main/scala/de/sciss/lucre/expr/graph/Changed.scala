/*
 *  Changed.scala
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
import de.sciss.model.Change

object Changed {
  private final class Expanded[T <: Exec[T], A](in: IExpr[T, A], tx0: T)
                                               (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    in.changed.--->(this)(tx0)

    override def toString: String = s"Changed($in)"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      val res: Option[Change[A]] = pull(in.changed)
      // println(s"Changed.pullUpdate: $res")
      val t = res.exists(_.isSignificant)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: T): Unit =
      in.changed -/-> changed
  }
}
final case class Changed[A](in: Ex[A]) extends Trig {
  type Repr[T <: Txn[T]] = ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val inExp = in.expand[T]
    new Changed.Expanded[T, A](inExp, tx)
  }
}
