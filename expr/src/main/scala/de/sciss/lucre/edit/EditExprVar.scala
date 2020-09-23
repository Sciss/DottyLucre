/*
 *  EditExprVar.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.edit.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.edit.impl.BasicUndoableEdit

object EditExprVar {
  def apply[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](vr: E[T] with Var[T, E[T]], value: E[T])
                                                         (implicit tx: T, tpe: Expr.Type[A, E]): Unit =
    UndoManager.find[T].fold(
      applyDo   [T, A, E](vr, value)
    ) { implicit undo =>
      applyUndo [T, A, E](vr, value)
    }

  def applyDo[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](vr: E[T] with Var[T, E[T]], value: E[T])
                                                           (implicit tx: T): Unit =
    vr() = value

  def applyUndo[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](vr: E[T] with Var[T, E[T]], value: E[T])
                                                             (implicit tx: T, tpe: Expr.Type[A, E],
                                                              undo: UndoManager[T]): Unit = {
    val edit = new Apply[T, A, E](vr, value, tx)
    undo.addEdit(edit)
  }

  private final class Apply[T <: Txn[T], A, E[~ <: Txn[~]] <: Expr[~, A]](vr0: E[T] with Var[T, E[T]],
                                                                          value0: E[T], tx0: T)
                                                                         (implicit tpe: Expr.Type[A, E])
    extends BasicUndoableEdit[T] {

    import tpe.{format, varFormat}

    private[this] val vrH       = tx0.newHandle(vr0)
    private[this] val valueH    = tx0.newHandle(value0)
    private[this] val prevH     = tx0.newHandle(vr0.swap(value0)(tx0))

    private def invalidMessage = s"$name: value changed"

    private def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def undoImpl()(implicit tx: T): Unit = {
      val vr    = vrH()
      val v     = valueH()
      val prev  = prevH()
      val found = vr.swap(prev)
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (found !== v) cannotUndo()
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val vr    = vrH()
      val v     = valueH()
      val prev  = prevH()
      val found = vr.swap(v)
      // we are quite conservative: if conditions changes such that value
      // was no longer as expected, we refuse the operation
      if (found !== prev) cannotRedo()
    }

    def name: String = "Update Variable"
  }
}
