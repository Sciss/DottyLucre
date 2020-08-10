/*
 *  Serializer.scala
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

package de.sciss
package serial

import de.sciss.serial.impl.{EitherSerializer, IndexedSeqSerializer, ListSerializer, MapSerializer, OptionSerializer, SetSerializer, Tuple2Serializer, Tuple3Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object Serializer {
  type Immutable[A] = Serializer[Any, Any, A]

  // ---- primitives ----

  implicit final object Unit extends ImmutableSerializer[scala.Unit] {
    def write(v: scala.Unit, out: DataOutput): Unit = ()

    def read(in: DataInput): Unit = ()
  }

  implicit final object Boolean extends ImmutableSerializer[scala.Boolean] {
    def write(v: scala.Boolean, out: DataOutput): Unit = out.writeBoolean(v)

    def read(in: DataInput): scala.Boolean = in.readBoolean()
  }

  implicit final object Char extends ImmutableSerializer[scala.Char] {
    def write(v: scala.Char, out: DataOutput): Unit = out.writeChar(v)

    def read(in: DataInput): scala.Char = in.readChar()
  }

  implicit final object Int extends ImmutableSerializer[scala.Int] {
    def write(v: scala.Int, out: DataOutput): Unit = out.writeInt(v)

    def read(in: DataInput): scala.Int = in.readInt()
  }

  implicit final object Float extends ImmutableSerializer[scala.Float] {
    def write(v: scala.Float, out: DataOutput): Unit = out.writeFloat(v)

    def read(in: DataInput): scala.Float = in.readFloat()
  }

  implicit final object Long extends ImmutableSerializer[scala.Long] {
    def write(v: scala.Long, out: DataOutput): Unit = out.writeLong(v)

    def read(in: DataInput): scala.Long = in.readLong()
  }

  implicit final object Double extends ImmutableSerializer[scala.Double] {
    def write(v: scala.Double, out: DataOutput): Unit = out.writeDouble(v)

    def read(in: DataInput): scala.Double = in.readDouble()
  }

  implicit final object String extends ImmutableSerializer[java.lang.String] {
    def write(v: java.lang.String, out: DataOutput): Unit = out.writeUTF(v)

    def read(in: DataInput): java.lang.String = in.readUTF()
  }

  implicit final object File extends ImmutableSerializer[java.io.File] {
    def write(v: java.io.File, out: DataOutput): Unit = out.writeUTF(v.getPath)

    def read(in: DataInput): java.io.File = new java.io.File(in.readUTF())
  }

  // ---- higher-kinded ----

  implicit def option[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Serializer[Tx, Acc, Option[A]] =
    new OptionSerializer[Tx, Acc, A](peer)

  implicit def either[Tx, Acc, A, B](implicit peer1: Serializer[Tx, Acc, A],
                                              peer2: Serializer[Tx, Acc, B]): Serializer[Tx, Acc, Either[A, B]] =
    new EitherSerializer[Tx, Acc, A, B](peer1, peer2)

  implicit def tuple2[Tx, Acc, A1, A2](implicit peer1: Serializer[Tx, Acc, A1],
                                                peer2: Serializer[Tx, Acc, A2]): Serializer[ Tx, Acc, (A1, A2) ] =
    new Tuple2Serializer[Tx, Acc, A1, A2](peer1, peer2)

  implicit def tuple3[Tx, Acc, A1, A2, A3](implicit peer1: Serializer[Tx, Acc, A1],
                                                    peer2: Serializer[Tx, Acc, A2],
                                                    peer3: Serializer[Tx, Acc, A3]): Serializer[Tx, Acc, (A1, A2, A3)] =
    new Tuple3Serializer[Tx, Acc, A1, A2, A3](peer1, peer2, peer3)

  implicit def list[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Serializer[Tx, Acc, List[A]] =
    new ListSerializer[Tx, Acc, A](peer)

  implicit def set[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Serializer[Tx, Acc, Set[A]] =
    new SetSerializer[Tx, Acc, A](peer)

  implicit def indexedSeq[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Serializer[Tx, Acc, Vec[A]] =
    new IndexedSeqSerializer[Tx, Acc, A](peer)

//  implicit def map[Tx, Acc, A, B](implicit peer: Serializer[Tx, Acc, (A, B)]): Serializer[Tx, Acc, Map[A, B]] =
//    new MapSerializer[Tx, Acc, A, B](peer)

  implicit def map[Tx, Acc, A, B](implicit key: Serializer[Tx, Acc, A],
                                  value: Serializer[Tx, Acc, B]): Serializer[Tx, Acc, Map[A, B]] =
    new MapSerializer[Tx, Acc, A, B](key, value)
}

trait Serializer[-Tx, -Acc, A]
  extends Reader[Tx, Acc, A] with Writer[A]
