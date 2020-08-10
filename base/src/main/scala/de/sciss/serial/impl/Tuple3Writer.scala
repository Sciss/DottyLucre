/*
 *  Tuple3Writer.scala
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

final class Tuple3Writer[A1, A2, A3](peer1: Writer[A1],
                                     peer2: Writer[A2],
                                     peer3: Writer[A3])
  extends Writer[(A1, A2, A3)] {

  def write(tup: (A1, A2, A3), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
    peer3.write(tup._3, out)
  }
}