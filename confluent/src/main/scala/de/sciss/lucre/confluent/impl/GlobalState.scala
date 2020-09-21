/*
 *  GlobalState.scala
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
package confluent
package impl

import de.sciss.lucre.data.Ancestor
import de.sciss.serial.{DataInput, DataOutput}

private[impl] object GlobalState {
  private val SER_VERSION = 0x436F6E666C6E7400L  // "Conflnt\0"

  implicit def serializer[T <: Txn[T], D <: DurableLike.Txn[D]]: TSerializer[D, GlobalState[T, D]] =
    new Ser[T, D]

  private final class Ser[T <: Txn[T], D <: DurableLike.Txn[D]]
    extends TSerializer[D, GlobalState[T, D]] {

    def write(v: GlobalState[T, D], out: DataOutput): Unit = {
      import v._
      out.writeLong(SER_VERSION)
      out.writeInt(durRootId) // writeInt(durRootId)
      idCnt        .write(out)
      versionLinear.write(out)
      versionRandom.write(out)
      partialTree  .write(out)
    }

    def read(in: DataInput, tx: D)(implicit acc: tx.Acc): GlobalState[T, D] = {
      val serVer = in.readLong()
      if (serVer != SER_VERSION)
        throw new IllegalStateException(s"Incompatible serialized version. Found $serVer but require $SER_VERSION")
      val durRootId     = in.readInt() // readInt()
      val idCnt         = tx.readCachedIntVar(in)
      val versionLinear = tx.readCachedIntVar(in)
      val versionRandom = tx.readCachedLongVar(in)
      val partialTree   = Ancestor.readTree[D, Long](in, tx)(acc, implicitly[TSerializer[D, Long]], _.toInt)
      GlobalState[T, D](durRootId = durRootId, idCnt = idCnt, versionLinear = versionLinear,
        versionRandom = versionRandom, partialTree = partialTree)
    }
  }
}

private[impl] final case class GlobalState[T <: Txn[T], D <: DurableLike.Txn[D]](
    durRootId: Int, idCnt: TVar[D, Int], versionLinear: TVar[D, Int], versionRandom: TVar[D, Long],
    partialTree:  Ancestor.Tree[D, Long])
