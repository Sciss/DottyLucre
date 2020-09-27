/*
 *  LoadBang.scala
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

import de.sciss.lucre.expr.{Context, IControl, ITrigger}
import de.sciss.lucre.impl.IGeneratorEvent
import de.sciss.lucre.{Exec, IEvent, IPull, ITargets, Txn}

object LoadBang {
  private final class Expanded[T <: Exec[T]](implicit protected val targets: ITargets[T])
    extends IControl[T] with ITrigger[T] with IGeneratorEvent[T, Unit] {

    override def toString: String = "LoadBang()"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] =
      Trig.Some

    def dispose()(implicit tx: T): Unit = ()

    def initControl()(implicit tx: T): Unit = fire(())
  }

}
final case class LoadBang() extends Control with Trig {
  type Repr[T <: Exec[T]] = IControl[T] with ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new LoadBang.Expanded[T]
  }
}
