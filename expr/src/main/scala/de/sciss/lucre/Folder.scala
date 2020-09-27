/*
 *  Folder.scala
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

import de.sciss.lucre.impl.{FolderImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}

object Folder extends Obj.Type {
  final val typeId = 0x10000

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  def apply[T <: Txn[T]]()(implicit tx: T): Folder[T] = Impl[T]()

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Folder[T] =
    format[T].readT(in)

  type Update [T <: Txn[T]]           = ListObj.Update [T, Obj[T], Folder[T]]
  type Change [T <: Txn[T]]           = ListObj.Change [Obj[T]]
  type Added  [T <: Txn[T]]           = ListObj.Added  [Obj[T]]
  val Added  : ListObj.Added   .type  = ListObj.Added
  type Removed[T <: Txn[T]]           = ListObj.Removed[Obj[T]]
  val Removed: ListObj.Removed .type  = ListObj.Removed

  implicit def format[T <: Txn[T]]: TFormat[T, Folder[T]] =
    Impl.format[T]
}
trait Folder[T <: Txn[T]] extends ListObj.Modifiable[T, Obj[T]] {
  /** This is simply because we inherit from `stm.List`. We refine the return type here. */
  override def modifiableOption: Option[Folder[T]]

  override def changed: EventLike[T, ListObj.Update[T, Obj[T], Folder[T]]]
}