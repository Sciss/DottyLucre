/*
 *  DataInput.scala
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

import java.io.{Closeable, DataInputStream, File, InputStream}

import de.sciss.serial.impl.{ByteArrayInputStream, FileWrapperImpl}

object DataInput {
  def apply(buf: Array[Byte]): DataInput with ByteArrayStream = apply(buf, 0, buf.length)
  def apply(buf: Array[Byte], off: Int, len: Int): DataInput with ByteArrayStream = {
    val bin = new ByteArrayInputStream(buf, off, len)
    new ByteArrayImpl(bin)
  }
  def open(file: File): DataInput with Closeable = new FileImpl(file)

  private final class ByteArrayImpl(bin: ByteArrayInputStream)
    extends DataInputStream(bin) with DataInput with ByteArrayStream {

    override def toString = s"DataInput(pos = $position, available = ${bin.available})@${hashCode().toHexString}"

    @inline def position        : Int         = bin.position
    @inline def position_=(value: Int): Unit  = bin.position = value

    @inline def toByteArray: Array[Byte] = bin.toByteArray

    @inline def size: Int = bin.size

    @inline def buffer: Array[Byte] = bin.buffer

    def asInputStream: InputStream = this
  }

  private final class FileImpl(file: File)
    extends FileWrapperImpl(file, "r") with DataInput
}
trait DataInput extends java.io.DataInput with RandomAccess {
  def asInputStream: InputStream
}
