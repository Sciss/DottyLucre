/*
 *  ExpandedObjMakeImpl.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.IAction
import de.sciss.lucre.expr.impl.ITriggerConsumer
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Caching, IChangeEvent, IExpr, IPush, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

/** Building block for expanded object's with Obj.Make functionality. */
abstract class ExpandedObjMakeImpl[T <: Txn[T], A](implicit protected val targets: ITargets[T])
  extends IExpr[T, A]
    with IAction[T]
    with IChangeGeneratorEvent [T, A]
    with ITriggerConsumer [T, A]
    with Caching {

  /** Must not be a `val` (used during initialization). */
  protected def empty: A

  protected def make()(implicit tx: T): A

  private[this] val ref = Ref[A](empty)

  def value(implicit tx: T): A =
    IPush.tryPull(this).fold(ref())(_.now)

  def executeAction()(implicit tx: T): Unit = {
    val ch = Change(valueBefore(), trigReceived())
    if (ch.isSignificant) fire(ch)
  }

  protected def trigReceived()(implicit tx: T): A = {
    val now = make()
    ref() = now
    now
  }

  protected def valueBefore()(implicit tx: T): A = ref()

  def changed: IChangeEvent[T, A] = this
}
