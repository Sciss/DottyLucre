/*
 *  Tuple2Serializer.scala
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

final class Tuple2Serializer[Tx, Acc, A1, A2](peer1: Serializer[Tx, Acc, A1],
                                              peer2: Serializer[Tx, Acc, A2])
  extends Serializer[Tx, Acc, (A1, A2)] {

  def write(tup: (A1, A2), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
  }

  def read(in: DataInput, acc: Acc)(implicit tx: Tx): (A1, A2) = {
    val a1 = peer1.read(in, acc)
    val a2 = peer2.read(in, acc)
    (a1, a2)
  }
}

final class ImmutableTuple2Serializer[A1, A2](peer1: ImmutableSerializer[A1],
                                              peer2: ImmutableSerializer[A2])
  extends ImmutableSerializer[(A1, A2)] {

  def write(tup: (A1, A2), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
  }

  def read(in: DataInput): (A1, A2) = {
    val a1 = peer1.read(in)
    val a2 = peer2.read(in)
    (a1, a2)
  }
}