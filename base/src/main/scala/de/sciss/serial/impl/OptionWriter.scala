/*
 *  OptionWriter.scala
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

final class OptionWriter[A](peer: Writer[A])
  extends Writer[Option[A]] {

  def write(opt: Option[A], out: DataOutput): Unit =
    opt match {
      case Some(v)  => out.writeByte(1); peer.write(v, out)
      case _        => out.writeByte(0)
    }
}
