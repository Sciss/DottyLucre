/*
 *  UndoableEdit.scala
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

package de.sciss.lucre.stm

trait UndoableEdit[T <: Exec[T]] extends Disposable[T] {
  def undo()(implicit tx: T): Unit
  def redo()(implicit tx: T): Unit

  /** Tries to merge this edit with a successive edit.
    *
    * @return   `Some` new edit containing both this and the successive edit, if possible,
    *           `None` if merging is not possible
    */
  def tryMerge(succ: UndoableEdit[T])(implicit tx: T): Option[UndoableEdit[T]]

  /** Whether this edit is destructive to the document model or not. */
  def significant: Boolean

  def name: String
}
