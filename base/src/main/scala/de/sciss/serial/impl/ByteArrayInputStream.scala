/*
 *  ByteArrayInputStream.scala
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

package de.sciss.serial.impl

import java.io.InputStream

import de.sciss.serial.ByteArrayStream

/** A non-synchronized alternative to `java.io.ByteArrayInputStream` */
final class ByteArrayInputStream(val buffer: Array[Byte], off: Int, val size: Int)
  extends InputStream with ByteArrayStream {

  private var _off = off

  def this(buf: Array[Byte]) = this(buf, 0, buf.length)

  def read(): Int = {
    if (_off >= size) return -1

    val b = buffer(_off) & 0xFF
    _off += 1
    b
  }

  def toByteArray: Array[Byte] = buffer.clone()

  override def read(out: Array[Byte], outOff: Int, outLen: Int): Int = {
    val rem = size - _off
    if (rem == 0) return -1
    val num = if (outLen > rem) rem else outLen

    System.arraycopy(buffer, _off, out, outOff, num)
    _off += num
    num
  }

  override def available: Int = size - _off

  def position: Int = _off
  def position_=(value: Int): Unit = {
    if (value < 0 || value > size) throw new IndexOutOfBoundsException(value.toString)
    _off = value
  }

  override def skip(n: Long): Long = {
    val rem = size - _off
    val res = if (n > rem) rem else n.toInt
    _off += res
    res
  }
}