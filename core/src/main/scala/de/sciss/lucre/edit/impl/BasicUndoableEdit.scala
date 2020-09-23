/*
 *  BasicUndoableEdit.scala
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

package de.sciss.lucre
package edit
package impl

import de.sciss.lucre.TxnLike.peer

import scala.concurrent.stm.Ref

abstract class BasicUndoableEdit[T <: Txn[T]] extends UndoableEdit[T] {

  // ---- abstract ----

  protected def undoImpl()(implicit tx: T): Unit
  protected def redoImpl()(implicit tx: T): Unit

  // ---- impl ----

  private[this] val state = Ref(0)    // 0 'done', '1' undone, '2' disposed

  final def undo()(implicit tx: T): Unit = {
    require (state.swap(1) == 0)
    undoImpl()
  }

  final def redo()(implicit tx: T): Unit = {
    require (state.swap(0) == 1)
    redoImpl()
  }

  /** Returns `None`. Implementations may override. */
  def tryMerge(succ: UndoableEdit[T])(implicit tx: T): Option[UndoableEdit[T]] = None

  /** Returns `true`. Implementations may override. */
  def significant: Boolean = true

  /** Can be overridden if `super` is called. */
  def dispose()(implicit tx: T): Unit = {
    state() = 2
  }
}
