/*
 *  UndoManager.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.impl.UndoManagerImpl

import scala.concurrent.stm.TxnLocal

object UndoManager {
  def dummy[T <: Txn[T]]: UndoManager[T] = UndoManagerImpl.dummy

  def apply[T <: Txn[T]](): UndoManager[T] =
    UndoManagerImpl()

  def using[T <: Txn[T], A](m: UndoManager[T])(body: => A)(implicit tx: T): A = {
    val before = current.swap(m)
    try {
      body
    } finally {
      current() = before
    }
  }

  def find[T <: Txn[T]](implicit tx: T): Option[UndoManager[T]] =
    Option(current().asInstanceOf[UndoManager[T]])

  final case class Update[T <: Txn[T]](m: UndoManager[T], undoName: Option[String], redoName: Option[String]) {
    def canUndo: Boolean = undoName.isDefined
    def canRedo: Boolean = redoName.isDefined
  }

  private val current = TxnLocal[UndoManager[_]]()

  final class CannotUndoException(message: String) extends RuntimeException(message)
  final class CannotRedoException(message: String) extends RuntimeException(message)
}
trait UndoManager[T <: Txn[T]] extends Disposable[T] with Observable[T, UndoManager.Update[T]] {
  /** Add another edit to the history.
    * Unless merging is blocked, it tries to merge this edit
    * with the most recent edit. Afterwards,
    * the internal merge-block flag is cleared.
    */
  def addEdit(edit: UndoableEdit[T])(implicit tx: T): Unit

  /** Creates a compound capture if during the execution of `block` more than
    * one edit is added. If exactly one edit is added, it will be directly
    * added without a wrapping compound.
    *
    * '''Note:''' it does not set the temporary global manager,
    * to do so, `UndoManager.using` must be called additionally!
    */
  def capture[A](name: String)(block: => A)(implicit tx: T): A

  /** Disallow the merging of the next edit to be added.
    * This can be used to avoid merging edits if the editor
    * component was temporarily unfocused, for example.
    */
  def blockMerge()(implicit tx: T): Unit

  /** Whether there are undoable edits and thus `undo` and ` undoName` may be called. */
  def canUndo(implicit tx: T): Boolean

  /** Throws an exception if `!canUndo` */
  def undo()(implicit tx: T): Unit

  /** Throws an exception if `!canUndo` */
  def undoName(implicit tx: T): String

  /** Whether there are edits that can be redone, and thus whether `redo` and ` redoName` may be called. */
  def canRedo(implicit tx: T): Boolean

  /** Throws an exception if `!canRedo` */
  def redo()(implicit tx: T): Unit

  /** Throws an exception if `!canRedo` */
  def redoName(implicit tx: T): String

  /** Clears the history, removing all edits. Afterwards, `canUndo` and `canRedo` will return `false`. */
  def clear()(implicit tx: T): Unit
}