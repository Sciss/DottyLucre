/*
 *  ITriggerConsumer.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.IPush.Parents
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.ITrigger
import de.sciss.lucre.{IChangePublisher, IPull, Txn}

import scala.concurrent.stm.Ref

/** A helper implementation for mapping trigger inputs to
 * outputs. Can be used for example with `IAction` and `IGenerator`.
 */
trait ITriggerConsumer[T <: Txn[T], A] extends IChangePublisher[T, A] {
  private[this] val inputs = Ref(List.empty[ITrigger[T]])

  def dispose()(implicit tx: T): Unit =
    inputs.swap(Nil).foreach { tr =>
      tr.changed -/-> this.changed
    }

  def addSource(tr: ITrigger[T])(implicit tx: T): Unit =
    tr.changed ---> this.changed

  protected def valueBefore ()(implicit tx: T): A
  protected def trigReceived()(implicit tx: T): A

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
    if (pull.isOrigin(this.changed)) pull.resolveChange[A]
    else if (phase.isBefore) valueBefore() else {
      val p: Parents[T] = pull.parents(this.changed)
      if (p.exists(pull(_).isDefined)) trigReceived() else valueBefore()
    }

  //  private[lucre] def pullUpdateXXX(pull: IPull[T])(implicit tx: T) : Option[Change[A]] = {
  //    if (pull.isOrigin(this.changed)) Some(pull.resolve)
  //    else {
  //      val p: Parents[T] = pull.parents(this.changed)
  //      if (p.exists(pull(_).isDefined)) trigReceived() else None
  //    }
  //  }
}
