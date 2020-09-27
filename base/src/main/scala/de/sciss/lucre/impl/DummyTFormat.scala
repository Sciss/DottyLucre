/*
 *  DummyTFormat.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.impl

import de.sciss.lucre.{AnyExec, Exec}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object DummyTFormat {
  def apply[T <: Exec[T], A]: TFormat[T, A] = anyFmt.asInstanceOf[TFormat[T, A]]

  private val anyFmt = new Impl[AnyExec, Nothing]

  private class Impl[T <: Exec[T], A]
    extends TFormat[T, A] {

    def write(v: A, out: DataOutput): Unit = ()

    def readT(in: DataInput)(implicit tx: T): A =
      sys.error("DummyTFormat.readT: Operation not supported")
  }
}