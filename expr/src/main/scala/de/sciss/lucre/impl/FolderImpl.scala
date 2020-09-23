/*
 *  FolderImpl.scala
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
package impl

import de.sciss.lucre.Event.Targets
import de.sciss.serial.{DataInput, TFormat}

object FolderImpl {
  def apply[T <: Txn[T]]()(implicit tx: T): Folder[T] =
    new Impl1[T] {
      protected val targets: Targets[T]   = Targets[T]()
      protected val sizeRef: Var[T, Int]  = id.newIntVar(0)
      protected val headRef: Var[T, C]    = id.newVar[C](null)(tx, CellFmt)
      protected val lastRef: Var[T, C]    = id.newVar[C](null)(tx, CellFmt)
    }

  def format[T <: Txn[T]]: TFormat[T, Folder[T]] =
    anyFmt.asInstanceOf[Fmt[T]]

  private val anyFmt = new Fmt[AnyTxn]

  // XXX TODO --- DRY - should make this public in expr.ListImpl
  private def copyList[In <: Txn[In], Out <: Txn[Out]](in : Folder[In ], out: Folder[Out])
                                                      (implicit txIn: In, txOut: Out,
                                                       context: Copy[In, Out]): Unit = {
    in.iterator.foreach { elem =>
      out.addLast(context(elem))
    }
  }

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Folder[T]] {
    def tpe: Obj.Type = Folder
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val targets = Targets.read[T](in)
    FolderImpl.read(in, targets)
  }

  private def read[T <: Txn[T]](in: DataInput, _targets: Targets[T])(implicit tx: T): Impl1[T] =
    new Impl1[T] {
      protected val targets: Targets[T]   = _targets
      protected val sizeRef: Var[T, Int]  = id.readIntVar(in)
      protected val headRef: Var[T, C]    = id.readVar[C](in)
      protected val lastRef: Var[T, C]    = id.readVar[C](in)
    }

  private abstract class Impl1[T <: Txn[T]]
    extends ListObjImpl.Impl[T, Obj, Impl1[T]] with Folder[T] {

    in =>

    final def tpe: Obj.Type = Folder

    override def toString = s"Folder$id"

    def modifiableOption: Option[Folder[T]] = Some(this)

    final override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      val out = FolderImpl[Out]()
      context.defer[ListAux](in, out)(copyList(in, out)(txIn, txOut, context))
      // .connect
      out
    }
  }
}
