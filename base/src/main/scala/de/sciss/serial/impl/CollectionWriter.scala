/*
 *  CollectionWriter.scala
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

// XXX size might be a slow operation on That...
final class CollectionWriter[A](peer: Writer[A]) extends Writer[Traversable[A]] {
  def write(coll: Traversable[A], out: DataOutput): Unit = {
    out.writeInt(coll.size)
    val ser = peer
    coll.foreach(ser.write(_, out))
  }
}