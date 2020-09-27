/*
 *  Trig.scala
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

import de.sciss.lucre.IPush.Parents
import de.sciss.lucre.expr.{Context, IAction, ITrigger, TrigOps}
import de.sciss.lucre.impl.IGeneratorEvent
import de.sciss.lucre.{IEvent, IPull, ITargets, Txn}

import scala.language.implicitConversions

object Trig {
  final val Some: Option[Unit] = scala.Some(())

  implicit def ops(t: Trig): TrigOps = new TrigOps(t)

  /** Creates a "standalone" trigger that can be activated as an action.
    */
  def apply(): Act with Trig = Impl()

  private final class Expanded[T <: Txn[T]](implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IAction[T] with IGeneratorEvent[T, Unit] {

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      if (pull.isOrigin(this)) Trig.Some
      else {
        val p: Parents[T] = pull.parents(this)
        if (p.exists(pull(_).isDefined)) Trig.Some else None
      }
    }

    def changed: IEvent[T, Unit] = this

    def executeAction()(implicit tx: T): Unit = fire(())

    def addSource(tr: ITrigger[T])(implicit tx: T): Unit =
      tr.changed ---> this

    def dispose()(implicit tx: T): Unit = ()
  }


  private final case class Impl() extends Act with Trig {
    override def productPrefix: String = "Trig" // serialization

    type Repr[T <: Txn[T]] = IAction[T] with ITrigger[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new Expanded[T]
    }
  }
}

/** A trigger element.
  *
  * ''Important:'' Implementation inadvertently run into trouble if they do not
  * extend `Trig.Lazy` to avoid "splitting" of the event paths. The only reason this
  * is not enforced is that implementation may already mixin lazy traits such as
  * `Control` (and by extension, `Widget`).
  */
trait Trig extends Lazy {
  type Repr[T <: Txn[T]] <: ITrigger[T]
}