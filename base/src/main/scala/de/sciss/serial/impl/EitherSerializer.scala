/*
 *  EitherSerializer.scala
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

final class EitherSerializer[Tx, Acc, A, B](peer1: Serializer[Tx, Acc, A],
                                            peer2: Serializer[Tx, Acc, B])
  extends Serializer[Tx, Acc, Either[A, B]] {

  def write(either: Either[A, B], out: DataOutput): Unit =
    either match {
      case Left (a) => out.writeByte(0); peer1.write(a, out)
      case Right(b) => out.writeByte(1); peer2.write(b, out)
    }

  def read(in: DataInput, acc: Acc)(implicit tx: Tx): Either[A, B] =
    in.readByte() match {
      case 0 => Left (peer1.read(in, acc))
      case 1 => Right(peer2.read(in, acc))
    }
}

final class ImmutableEitherSerializer[A, B](peer1: ImmutableSerializer[A],
                                            peer2: ImmutableSerializer[B])
  extends ImmutableSerializer[Either[A, B]] {

  def write(either: Either[A, B], out: DataOutput): Unit =
    either match {
      case Left (a) => out.writeByte(0); peer1.write(a, out)
      case Right(b) => out.writeByte(1); peer2.write(b, out)
    }

  def read(in: DataInput): Either[A, B] =
    in.readByte() match {
      case 0 => Left (peer1.read(in))
      case 1 => Right(peer2.read(in))
    }
}
