/*
 *  EditArtifact.scala
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

package de.sciss.lucre
package edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.Artifact.Child
import de.sciss.lucre.edit.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.edit.impl.BasicUndoableEdit

object EditArtifact {
  def updateChild[T <: Txn[T]](a: Artifact.Modifiable[T], child: Child)(implicit tx: T): Unit =
    UndoManager.find[T].fold(
      updateChildDo   (a, child)
    ) { implicit undo =>
      updateChildUndo (a, child)
    }

  private def updateChildDo[T <: Txn[T]](a: Artifact.Modifiable[T], child: Child)(implicit tx: T): Unit =
    a.child = child

  def updateChildUndo[T <: Txn[T]](a: Artifact.Modifiable[T], child: Child)
                                  (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new UpdateChild[T](a, child, tx)
    undo.addEdit(edit)
  }

  private final class UpdateChild[T <: Txn[T]](a0: Artifact.Modifiable[T], child: Child, tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val aH    = tx0.newHandle(a0)
    private[this] val prev  = a0.child(tx0)

    private def invalidMessage = s"$name: value changed"

    private def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def undoImpl()(implicit tx: T): Unit = {
      val a     = aH()
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (a.child !== child) cannotUndo()
      a.child   = prev
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val a     = aH()
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (a.child !== prev) cannotRedo()
      a.child   = child
    }

    def name: String = "Update Artifact"
  }
}
