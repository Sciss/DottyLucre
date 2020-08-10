/*
 *  EitherWriter.scala
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

final class EitherWriter[A, B](peer1: Writer[A], peer2: Writer[B])
  extends Writer[Either[A, B]] {

  def write(either: Either[A, B], out: DataOutput): Unit =
    either match {
      case Left (a) => out.writeByte(0); peer1.write(a, out)
      case Right(b) => out.writeByte(1); peer2.write(b, out)
    }
}
