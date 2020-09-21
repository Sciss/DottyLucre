/*
 *  ObjImpl.scala
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

import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.meta.field

object ObjImpl {
  def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] = {
    val typeId  = in.readInt()
    val tpe     = getType(typeId)
    tpe.readIdentifiedObj(in, tx)
  }

  implicit def serializer[T <: Txn[T]]: TSerializer[T, Obj[T]] = anySer.asInstanceOf[Ser[T]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[AnyTxn]

  @volatile private var map = Map[Int, Obj.Type](TMap.typeId -> TMap)

  def addType(tpe: Obj.Type): Unit = sync.synchronized {
    val typeId = tpe.typeId
    if (map.contains(typeId))
      throw new IllegalArgumentException(
        s"Object type $typeId (0x${typeId.toHexString}) was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  @inline
  def getType(id: Int): Obj.Type = map.getOrElse(id, sys.error(s"Unknown object type $id (0x${id.toHexString})"))

  private final class Ser[T <: Txn[T]] extends TSerializer[T, Obj[T]] {
    def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] = ObjImpl.read(in, tx)

    def write(obj: Obj[T], out: DataOutput): Unit = obj.write(out)
  }
}
