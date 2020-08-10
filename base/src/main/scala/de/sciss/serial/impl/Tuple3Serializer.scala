/*
 *  Tuple3Serializer.scala
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

final class Tuple3Serializer[Tx, Acc, A1, A2, A3](peer1: Serializer[Tx, Acc, A1],
                                                  peer2: Serializer[Tx, Acc, A2],
                                                  peer3: Serializer[Tx, Acc, A3])
  extends Serializer[Tx, Acc, (A1, A2, A3)] {

  def write(tup: (A1, A2, A3), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
    peer3.write(tup._3, out)
  }

  def read(in: DataInput, acc: Acc)(implicit tx: Tx): (A1, A2, A3) = {
    val a1 = peer1.read(in, acc)
    val a2 = peer2.read(in, acc)
    val a3 = peer3.read(in, acc)
    (a1, a2, a3)
  }
}

final class ImmutableTuple3Serializer[A1, A2, A3](peer1: ImmutableSerializer[A1],
                                                  peer2: ImmutableSerializer[A2],
                                                  peer3: ImmutableSerializer[A3])
  extends ImmutableSerializer[(A1, A2, A3)] {

  def write(tup: (A1, A2, A3), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
    peer3.write(tup._3, out)
  }

  def read(in: DataInput): (A1, A2, A3) = {
    val a1 = peer1.read(in)
    val a2 = peer2.read(in)
    val a3 = peer3.read(in)
    (a1, a2, a3)
  }
}
