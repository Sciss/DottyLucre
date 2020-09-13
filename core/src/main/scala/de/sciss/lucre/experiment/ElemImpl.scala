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

package de.sciss.lucre.experiment

import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataOutput, DataInput, Serializer}

import scala.annotation.meta.field

object ElemImpl {
  def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Elem[T] = {
    val typeId  = in.readInt()
    val tpe     = getType(typeId)
    tpe.readIdentifiedObj(in, tx)
  }

  implicit def serializer[T <: Txn[T]]: TxSerializer[T, Elem[T]] = anySer.asInstanceOf[Ser[T]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[AnyTxn]

  // @volatile private var map = Map.empty[Int, Elem.Type]
  @volatile private var map = Map[Int, Elem.Type](???) // evt.Map.typeId -> evt.Map)

  def addType(tpe: Elem.Type): Unit = sync.synchronized {
    val typeId = tpe.typeId
    if (map.contains(typeId))
      throw new IllegalArgumentException(
        s"Element type $typeId (0x${typeId.toHexString}) was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  @inline
  def getType(id: Int): Elem.Type = map.getOrElse(id, sys.error(s"Unknown element type $id (0x${id.toHexString})"))

  private final class Ser[T <: Txn[T]] extends TxSerializer[T, Elem[T]] {
    def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Elem[T] = ElemImpl.read(in, tx)

    def write(obj: Elem[T], out: DataOutput): Unit = obj.write(out)
  }
}