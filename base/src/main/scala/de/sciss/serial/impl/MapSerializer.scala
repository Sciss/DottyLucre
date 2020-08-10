/*
 *  MapSerializer.scala
 *  (Serial)
 *
 * Copyright (c) 2011-2020 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.serial
package impl

final class MapSerializer[Tx, Acc, A, B](key  : Serializer[Tx, Acc, A],
                                         value: Serializer[Tx, Acc, B])
  extends Serializer[Tx, Acc, Map[A, B]] {

  def write(coll: Map[A, B], out: DataOutput): Unit = {
    val sz = coll.size
    out.writeInt(coll.size)
    if (sz > 0) {
      coll.foreach { tup =>
        key  .write(tup._1, out)
        value.write(tup._2, out)
      }
    }
  }

  def read(in: DataInput, acc: Acc)(implicit tx: Tx): Map[A, B] = {
    val sz = in.readInt()
    if (sz == 0) Map.empty
    else {
      val b = Map.newBuilder[A, B]
      b.sizeHint(sz)
      var rem = sz
      while (rem > 0) {
        val _1 = key  .read(in, acc)
        val _2 = value.read(in, acc)
        b += ((_1, _2))
        rem -= 1
      }
      b.result()
    }
  }
}

final class ImmutableMapSerializer[A, B](key  : ImmutableSerializer[A],
                                         value: ImmutableSerializer[B])
  extends ImmutableSerializer[Map[A, B]] {

  def write(coll: Map[A, B], out: DataOutput): Unit = {
    val sz = coll.size
    out.writeInt(coll.size)
    if (sz > 0) {
      coll.foreach { tup =>
        key  .write(tup._1, out)
        value.write(tup._2, out)
      }
    }
  }

  def read(in: DataInput): Map[A, B] = {
    val sz = in.readInt()
    if (sz == 0) Map.empty
    else {
      val b = Map.newBuilder[A, B]
      b.sizeHint(sz)
      var rem = sz
      while (rem > 0) {
        val _1 = key  .read(in)
        val _2 = value.read(in)
        b += ((_1, _2))
        rem -= 1
      }
      b.result()
    }
  }
}