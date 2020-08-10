/*
 *  FileWrapperImpl.scala
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

import java.io.{File, InputStream, OutputStream, RandomAccessFile}

private[impl] final class FileInputStreamImpl(peer: RandomAccessFile)
  extends InputStream {

  def read(): Int = peer.read()
  override def read(b: Array[Byte], off: Int, len: Int): Int = peer.read(b, off, len)
  override def skip(n: Long): Long = {
    val oldPos  = peer.getFilePointer
    val newPos  = math.min(peer.length(), oldPos + n)
    peer.seek(newPos)
    newPos - oldPos
  }
  override def available(): Int = { math.min(0x7FFFFFFFL, peer.length() - peer.getFilePointer).toInt }
  override def close(): Unit = peer.close()
}

private[impl] final class FileOutputStreamImpl(peer: RandomAccessFile) extends OutputStream {
  def write(b: Int): Unit = peer.write(b)

  override def write(b: Array[Byte], off: Int, len: Int): Unit = peer.write(b, off, len)

  override def close(): Unit = peer.close()
}

private[serial] abstract class FileWrapperImpl(file: File, mode: String)
  extends RandomAccessFile(file, mode) with DataInput with DataOutput {

  final def asInputStream : InputStream  = new FileInputStreamImpl (this)
  final def asOutputStream: OutputStream = new FileOutputStreamImpl(this)

  final def size: Int = {
    val n = length()
    require(n <= 0x7FFFFFFFL, "File too large")
    n.toInt
  }

  final def position: Int = {
    val n = getFilePointer
    require(n <= 0x7FFFFFFFL, "File too large")
    n.toInt
  }

  final def position_=(value: Int): Unit = seek(value)
}