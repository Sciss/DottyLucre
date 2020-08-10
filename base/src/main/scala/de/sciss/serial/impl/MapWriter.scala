/*
 *  MapWriter.scala
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

final class MapWriter[A, B](key: Writer[A], value: Writer[B]) extends Writer[Map[A, B]] {
  def write(coll: Map[A, B], out: DataOutput): Unit = {
    out.writeInt(coll.size)
    coll.foreach { tup =>
      key  .write(tup._1, out)
      value.write(tup._2, out)
    }
  }
}