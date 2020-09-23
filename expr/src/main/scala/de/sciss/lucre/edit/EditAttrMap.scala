/*
 *  EditAttrMap.scala
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

package de.sciss.lucre.edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.Obj.AttrMap
import de.sciss.lucre.edit.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.edit.impl.BasicUndoableEdit
import de.sciss.lucre.{Obj, Txn}

object EditAttrMap {
  def put[T <: Txn[T]](map: AttrMap[T], key: String, value: Obj[T])(implicit tx: T): Unit =
    UndoManager.find[T].fold(
      putDo  (map, key, value)
    ) { implicit undo =>
      putUndo(map, key, value)
    }

  private def putDo[T <: Txn[T]](map: AttrMap[T], key: String, value: Obj[T])(implicit tx: T): Unit =
    map.put(key, value)

  def putUndo[T <: Txn[T]](map: AttrMap[T], key: String, value: Obj[T])(implicit tx: T,
                                                                        undo: UndoManager[T]): Unit = {
    val edit = new Put(map, key, value, tx)
    undo.addEdit(edit)
  }

  def remove[T <: Txn[T]](map: AttrMap[T], key: String)(implicit tx: T): Unit =
    UndoManager.find[T].fold(
      removeDo  (map, key)
    ) { implicit undo =>
      removeUndo(map, key)
    }

  private def removeDo[T <: Txn[T]](map: AttrMap[T], key: String)(implicit tx: T): Unit =
    map.remove(key)

  def removeUndo[T <: Txn[T]](map: AttrMap[T], key: String)(implicit tx: T,
                                                            undo: UndoManager[T]): Unit = {
    val edit = new Remove(map, key, tx)
    undo.addEdit(edit)
  }

  private abstract class PutRemove[T <: Txn[T]] extends BasicUndoableEdit[T] {
    private def invalidMessage = s"$name: value in map changed"

    final protected def cannotUndo(): Nothing =
      throw new CannotUndoException(invalidMessage)

    final protected def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)
  }

  private final class Put[T <: Txn[T]](map0: AttrMap[T], key: String, value0: Obj[T], tx0: T)
    extends PutRemove[T] {

    private[this] val mapH      = tx0.newHandle(map0  )
    private[this] val valueH    = tx0.newHandle(value0)
    private[this] val prevHOpt  = map0.put(key, value0)(tx0).map(v => tx0.newHandle(v))

    protected def undoImpl()(implicit tx: T): Unit = {
      val m         = mapH  ()
      val v         = valueH()
      val foundOpt  = prevHOpt match {
        case Some(prevH)  => m.put(key, prevH())
        case None         => m.remove(key)
      }
      // we are quite conservative: if conditions changes such that value
      // was no longer at the key, we refuse the operation
      if (!foundOpt.contains(v)) cannotUndo()
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val m         = mapH()
      val v         = valueH()
      val foundOpt  = m.put(key, v)
      val prevOpt   = prevHOpt.map(_.apply())
      // we are quite conservative: if conditions changes such that previous value
      // is not the same as in the initial run, we refuse the operation
      if (foundOpt !== prevOpt) cannotRedo()
    }

    def name: String = s"Set Attribute $key"
  }

  private final class Remove[T <: Txn[T]](map0: AttrMap[T], key: String, tx0: T)
    extends PutRemove[T] {

    private[this] val mapH      = tx0.newHandle(map0  )
    private[this] val prevHOpt  = map0.remove(key)(tx0).map(v => tx0.newHandle(v))

    protected def undoImpl()(implicit tx: T): Unit = {
      val m         = mapH()
      val foundOpt  = prevHOpt match {
        case Some(prevH)  => m.put(key, prevH())
        case None         => None
      }
      // we are quite conservative: if conditions changes such that a value
      // was now present at the key, we refuse the operation
      if (foundOpt.isDefined) cannotUndo()
    }

    protected def redoImpl()(implicit tx: T): Unit = {
      val m         = mapH()
      val foundOpt  = m.remove(key)
      val prevOpt   = prevHOpt.map(_.apply())
      // we are quite conservative: if conditions changes such that previous value
      // is not the same as in the initial run, we refuse the operation
      if (foundOpt !== prevOpt) cannotRedo()
    }

    def name: String = s"Remove Attribute $key"
  }
}
