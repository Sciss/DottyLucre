/*
 *  DataOutput.scala
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

import java.io.{Closeable, DataOutputStream, File, OutputStream}

import de.sciss.serial.impl.{ByteArrayOutputStream, FileWrapperImpl}

object DataOutput {
  def apply(): DataOutput with ByteArrayStream = {
    val bout = new ByteArrayOutputStream()
    new ByteArrayImpl(bout)
  }

  def open(file: File): DataOutput with Closeable = new FileImpl(file)

  private final class ByteArrayImpl(bout: ByteArrayOutputStream)
    extends DataOutputStream(bout) with DataOutput with ByteArrayStream {

    override def toString = s"DataOutput.ByteArray(size = ${bout.size})@${hashCode().toHexString}"

    @inline def toByteArray: Array[Byte] = bout.toByteArray

    @inline def reset(): Unit = {
      bout.reset()
      written = 0
    }

    @inline def buffer: Array[Byte] = bout.buffer

    @inline def position: Int = bout.position

    @inline def position_=(value: Int): Unit = {
      bout.position = value
      // ideally, `written` would not exist, and we could just direct `size` to `bout.size`, but unfortunately
      // `size` is final. so all we can do to minimise damage, is reset written to the buffer offset. that
      // way `size` on this object will not include bytes after the write position. but that should be
      // consensus anyway.
      written       = value
    }

    def asOutputStream: OutputStream = this
  }

  private final class FileImpl(file: File) extends FileWrapperImpl(file, "rw") with DataOutput
}
trait DataOutput extends java.io.DataOutput with RandomAccess {
  def asOutputStream: OutputStream
}
