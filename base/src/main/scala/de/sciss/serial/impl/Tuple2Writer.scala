/*
 *  Tuple2Writer.scala
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

final class Tuple2Writer[A1, A2](peer1: Writer[A1], peer2: Writer[A2])
  extends Writer[(A1, A2) ] {

  def write(tup: (A1, A2), out: DataOutput): Unit = {
    peer1.write(tup._1, out)
    peer2.write(tup._2, out)
  }
}