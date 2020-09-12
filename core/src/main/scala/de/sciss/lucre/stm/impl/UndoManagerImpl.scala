/*
 *  UndoManagerImpl.scala
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

package de.sciss.lucre.stm.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException, Update}
import de.sciss.lucre.stm.{AnyTxn, UndoManager, UndoableEdit, Txn}

import scala.concurrent.stm.Ref

object UndoManagerImpl {
  def apply[T <: Txn[T]](): UndoManager[T] = new Impl[T]
  def dummy[T <: Txn[T]]  : UndoManager[T] = anyDummy.asInstanceOf[UndoManager[T]]

  private val anyDummy = new Dummy[AnyTxn]

  private final class Dummy[T <: Txn[T]] extends UndoManager[T] with DummyObservableImpl[T] {
    def addEdit(edit: UndoableEdit[T])(implicit tx: T): Unit = ()

    def capture[A](name: String)(block: => A)(implicit tx: T): A = block

    def blockMerge()(implicit tx: T): Unit = ()

    private def cannotUndo(): Nothing =  throw new CannotUndoException("Dummy undo manager")
    private def cannotRedo(): Nothing =  throw new CannotRedoException("Dummy undo manager")

    def canUndo (implicit tx: T): Boolean  = false
    def undo()  (implicit tx: T): Unit     = cannotUndo()
    def undoName(implicit tx: T): String   = cannotUndo()
    def canRedo (implicit tx: T): Boolean  = false
    def redo()  (implicit tx: T): Unit     = cannotRedo()
    def redoName(implicit tx: T): String   = cannotRedo()

    def clear   ()(implicit tx: T): Unit = ()
    def dispose ()(implicit tx: T): Unit = ()
  }

  private final class Impl[T <: Txn[T]]
    extends UndoManager[T] with ObservableImpl[T, UndoManager.Update[T]] { impl =>

    private[this] val toUndo    = Ref[List[UndoableEdit[T]]](Nil)
    private[this] val toRedo    = Ref[List[UndoableEdit[T]]](Nil)

    private[this] val _undoName = Ref(Option.empty[String])
    private[this] val _redoName = Ref(Option.empty[String])

    //    private[this] val busy        = Ref(false)
    private[this] val _blockMerge = Ref(false)

    def blockMerge()(implicit tx: T): Unit =
      _blockMerge() = true

    private[this] val Empty = new Compound[T]("", Nil, significant = false)

    // add a non-significant edit puts it into pending limbo,
    // because we do not yet want to purge the redo tree at this stage
    private[this] val pending = Ref(Empty)

    private[this] val capturing = Ref(Option.empty[Compound[T]])

    def capture[A](name: String)(block: => A)(implicit tx: T): A = {
      val c0        = new Compound[T](name, Nil, significant = false)
      val before    = capturing.swap(Some(c0))
      val res       = block
      val Some(now) = capturing.swap(before)
      if (now.nonEmpty) addEdit(now)
      res
    }

    def addEdit(edit: UndoableEdit[T])(implicit tx: T): Unit =
      capturing() match {
        case Some(c)  => capturing() = Some(c.merge(edit))
        case None     => addRegularEdit(edit)
      }

    private def addRegularEdit(edit: UndoableEdit[T])(implicit tx: T): Unit = {
      if (edit.significant) {
        val undoNameOld = _undoName()
        val toUndoOld   = toUndo()
        val canUndoOld  = toUndoOld.nonEmpty
        val _pending    = pending.swap(Empty)
        val toUndoTmp   = if (_pending.isEmpty) toUndoOld else _pending :: toUndoOld

        val toUndoNew = toUndoTmp match {
          case head :: tail if !_blockMerge() =>
            head.tryMerge(edit) match {
              case Some(merged) => merged :: tail
              case None         => edit   :: toUndoTmp
            }

          case _ => edit :: toUndoTmp
        }
        toUndo()        = toUndoNew
        val undoNameNew = toUndoNew.head.name
        _undoName()     = Some(undoNameNew)
        val toRedoOld   = toRedo.swap(Nil)
        val canRedoOld  = toRedoOld.nonEmpty
        _redoName()     = None
        toRedoOld.foreach(_.dispose())

        val _fire = !canUndoOld || canRedoOld || (canUndoOld && !undoNameOld.contains(undoNameNew))
        if (_fire) fire(Update(impl, undoName = Some(undoNameNew), redoName = None)) // notifyObservers()

      } else {
        if (canUndo) pending.transform(_.merge(edit))
      }

      _blockMerge() = false
    }

    def canUndo(implicit tx: T): Boolean = toUndo().nonEmpty

    def undo()(implicit tx: T): Unit = {
      val _pending = pending.swap(Empty)
      if (_pending.nonEmpty) {
        _pending.undo()
      } else {
        toUndo() match {
          case action :: tail =>
            action.undo()
            val toRedoOld   = toRedo.getAndTransform(action :: _)
            val canRedoOld  = toRedoOld.nonEmpty
            val undoNameOld = _undoName()
            val redoNameOld = _redoName()
            toUndo() = tail
            val canUndoNew  = tail.nonEmpty

            if (action.significant) {
              _redoName()   = Some(action.name)
              _undoName()   = tail match {
                case head :: _ if head.significant  => Some(head.name)
                case _ :: pen :: _                  => Some(pen .name)
                case _                              => None
              }
            }

            val _fire = !canUndoNew || !canRedoOld || (_undoName() !== undoNameOld) || (_redoName() !== redoNameOld)
            blockMerge()
            if (_fire) fire(Update(impl, undoName = _undoName(), redoName = _redoName()))

          case _ =>
            throw new IllegalStateException("Nothing to undo")
        }
      }
    }

    def undoName(implicit tx: T): String = _undoName().get

    def canRedo(implicit tx: T): Boolean = toRedo().nonEmpty

    def redo()(implicit tx: T): Unit = {
      val _pending = pending.swap(Empty)
      if (_pending.nonEmpty) {
        _pending.undo()
      }
      toRedo() match {
        case action :: tail =>
          action.redo()
          val toUndoOld   = toUndo.getAndTransform(action :: _)
          val canUndoOld  = toUndoOld.nonEmpty
          val undoNameOld = _undoName()
          val redoNameOld = _redoName()
          toRedo()        = tail
          val canRedoNew  = tail.nonEmpty
          if (action.significant) {
            _undoName()   = Some(action.name)
            _redoName()   = tail match {
              case head :: _ if head.significant  => Some(head.name)
              case _ :: pen :: _                  => Some(pen .name)
              case _                              => None
            }
          }

          val _fire = !canRedoNew || !canUndoOld || (_undoName() !== undoNameOld) || (_redoName() !== redoNameOld)
          blockMerge()
          if (_fire) fire(Update(impl, undoName = _undoName(), redoName = _redoName()))

        case _ =>
          throw new IllegalStateException("Nothing to redo")
      }
    }

    def redoName(implicit tx: T): String = _redoName().get

    def clear()(implicit tx: T): Unit = {
      if (clearNoFire()) fire(Update(impl, undoName = None, redoName = None))
    }

    private def clearNoFire()(implicit tx: T): Boolean = {
      val hadUndo = toUndo.swap(Nil).nonEmpty
      val _redo   = toRedo.swap(Nil)
      val hadRedo = _redo.nonEmpty
      _redo.foreach(_.dispose())
      hadUndo || hadRedo
    }

    def dispose()(implicit tx: T): Unit = {
      clearNoFire()
    }
  }

  private class Compound[T <: Txn[T]](val name: String, val edits: List[UndoableEdit[T]], val significant: Boolean)
    extends UndoableEdit[T] {

    def isEmpty : Boolean = edits.isEmpty
    def nonEmpty: Boolean = edits.nonEmpty

    def dispose()(implicit tx: T): Unit =
      edits.foreach(_.dispose())

    private def mergeEdits(succ: List[UndoableEdit[T]])(implicit tx: T): List[UndoableEdit[T]] =
      (succ, edits) match {
        case (init :+ succLast, head :: tail) =>
          head.tryMerge(succLast) match {
            case Some(merge)  => init ::: merge :: tail
            case None         => succ ::: edits
          }

        case _ => succ ::: edits
      }

    def merge(succ: UndoableEdit[T])(implicit tx: T): Compound[T] = {
      val succEdits = succ match {
        case a: Compound[T] => a.edits
        case _              => succ :: Nil
      }
      val newEdits = mergeEdits(succEdits)
      new Compound(name, newEdits, significant = significant || succ.significant)
    }

    def undo()(implicit tx: T): Unit =
      edits.foreach(_.undo())

    def redo()(implicit tx: T): Unit =
      edits.reverse.foreach(_.redo())

    def tryMerge(succ: UndoableEdit[T])(implicit tx: T): Option[UndoableEdit[T]] = succ match {
      case that: Compound[T] if !this.significant && !that.significant =>
        val newEdits = mergeEdits(that.edits)
        val m = new Compound(name, edits = newEdits, significant = significant)
        Some(m)

      case _ => None
    }
  }
}