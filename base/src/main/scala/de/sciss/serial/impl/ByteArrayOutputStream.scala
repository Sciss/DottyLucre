/*
 *  ByteArrayOutputStream.scala
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

import java.io.OutputStream

import de.sciss.serial.ByteArrayStream

/**
 * A non-synchronized alternative to `java.io.ByteArrayOutputStream`.
 */
object ByteArrayOutputStream {
  private final val emptyBuf = new Array[Byte](0)
}
final class ByteArrayOutputStream(initialSize: Int = 128) extends OutputStream with ByteArrayStream {
  private var buf = new Array[Byte](initialSize)
  private var _pos = 0
  private var _len = 0

  def size: Int = _len

  def reset(): Unit = {
    _pos = 0
    _len = 0
  }

  def position: Int = _pos
  def position_=(value: Int): Unit = {
    if (value < 0 || value > _len) throw new IndexOutOfBoundsException(value.toString)
    _pos = value
  }

  def write(b: Int): Unit = {
    if (_pos == buf.length) alloc(1)
    buf(_pos)  = b.toByte
    _pos += 1
    if (_pos > _len) _len = _pos
  }

  override def write(in: Array[Byte], inOff: Int, inLen: Int): Unit = {
    val needed = _pos + inLen - buf.length
    if (needed > 0) alloc(needed)

    System.arraycopy(in, inOff, buf, _pos, inLen)
    _pos += inLen
    if (_pos > _len) _len = _pos
  }

  def toByteArray: Array[Byte] = {
    if (_len == 0) return ByteArrayOutputStream.emptyBuf

    val res = new Array[Byte](_len)
    System.arraycopy(buf, 0, res, 0, _len)
    res
  }

  /** Returns the current underlying buffer. This is a shared mutable buffer, so use this
    * cautiously where no other object can access this stream.
    */
  def buffer: Array[Byte] = buf

  private def alloc(needed: Int): Unit = {
    val newLen = (buf.length << 1) + needed
    val newBuf = new Array[Byte](newLen)
    System.arraycopy(buf, 0, newBuf, 0, _len)
    buf = newBuf
  }
}
