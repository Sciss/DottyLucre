/*
 *  Implicits.scala
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

object Implicits {
  implicit object writerFromWritable extends Writer[Writable] {
    def write(w: Writable, out: DataOutput): Unit = w.write(out)
  }

  implicit def serializerFromWritableAndReader[Tx, Acc, A <: Writable](implicit reader: Reader[Tx, Acc, A]): Serializer[Tx, Acc, A] =
    new ReaderWrapper(reader)

  private final class ReaderWrapper[Tx, Acc, A <: Writable](reader: Reader[Tx, Acc, A]) extends Serializer[Tx, Acc, A] {
    def write(v: A, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: Acc)(implicit tx: Tx): A = reader.read(in, access)
  }
}