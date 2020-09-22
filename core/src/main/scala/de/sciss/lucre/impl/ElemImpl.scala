/*
 *  ElemImpl.scala
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

import de.sciss.serial.DataInput

import scala.annotation.meta.field

object ElemImpl {
  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Elem[T] = {
    val typeId  = in.readInt()
    val tpe     = getType(typeId)
    tpe.readIdentifiedObj(in)
  }

  implicit def serializer[T <: Txn[T]]: TSerializer[T, Elem[T]] = anySer.asInstanceOf[Ser[T]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[AnyTxn]

  // @volatile private var map = Map.empty[Int, Elem.Type]
  @volatile private var map = Map[Int, Elem.Type](TMap.typeId -> TMap)

  def addType(tpe: Elem.Type): Unit = sync.synchronized {
    val typeId = tpe.typeId
    if (map.contains(typeId))
      throw new IllegalArgumentException(
        s"Element type $typeId (0x${typeId.toHexString}) was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  @inline
  def getType(id: Int): Elem.Type = map.getOrElse(id, sys.error(s"Unknown element type $id (0x${id.toHexString})"))

  private final class Ser[T <: Txn[T]] extends WritableSerializer[T, Elem[T]] {
    override def readT(in: DataInput)(implicit tx: T): Elem[T] = ElemImpl.read(in)
  }
}