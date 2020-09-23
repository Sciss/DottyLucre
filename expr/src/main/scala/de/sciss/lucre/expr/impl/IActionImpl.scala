/*
 *  IActionImpl.scala
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
package impl

import de.sciss.lucre.TxnLike.peer
import de.sciss.lucre.{Disposable, Txn}

import scala.concurrent.stm.Ref

trait IActionImpl[T <: Txn[T]] extends IAction[T] {
  private[this] val disposables = Ref(List.empty[Disposable[T]])

  protected final def addDisposable(d: Disposable[T])(implicit tx: T): Unit =
    disposables.transform(d :: _)

  def dispose()(implicit tx: T): Unit =
    disposables.swap(Nil).foreach(_.dispose())

  def addSource(tr: ITrigger[T])(implicit tx: T): Unit = {
    val obs = tr.changed.react { implicit tx => _ => executeAction() }
    addDisposable(obs)
  }
}
