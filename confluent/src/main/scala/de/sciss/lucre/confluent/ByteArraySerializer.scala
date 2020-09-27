/*
 *  ByteArrayFormat.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent

import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

object ByteArrayFormat extends ConstFormat[Array[Byte]] {
  def write(v: Array[Byte], out: DataOutput): Unit = {
    out./* PACKED */ writeInt(v.length)
    out.write(v)
  }

  def read(in: DataInput): Array[Byte] = {
    val sz = in./* PACKED */ readInt()
    val v = new Array[Byte](sz)
    in.readFully(v)
    v
  }
}