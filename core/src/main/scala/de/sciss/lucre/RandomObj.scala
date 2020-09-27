/*
 *  RandomObj.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import de.sciss.lucre.impl.CastExecFormat
import de.sciss.lucre.impl.RandomImpl.{SysLike, calcSeedUniquifier, initialScramble}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object RandomObj {
  private final class Impl[T <: Exec[T]](val id: Ident[T], protected val seedRef: Var[T, Long])
    extends SysLike[T] with RandomObj[T] {

    override def copy[Out <:  Exec[Out]]()(implicit tx: T, txOut: Out): RandomObj[Out] = {
      val idOut       = txOut.newId()
      val seedRefOut  = idOut.newLongVar(seedRef())
      new Impl[Out](id = idOut, seedRef = seedRefOut)
    }

    def write(out: DataOutput): Unit = {
      id     .write(out)
      seedRef.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id     .dispose()
      seedRef.dispose()
    }
  }

  def apply[T <: Exec[T]]()(implicit tx: T): RandomObj[T] =
    apply(calcSeedUniquifier() ^ System.nanoTime())

  def apply[T <: Exec[T]](seed: Long)(implicit tx: T): RandomObj[T] = {
    val id = tx.newId()
    new Impl[T](id, id.newLongVar(initialScramble(seed)))
  }

  def read[T <: Exec[T]](in: DataInput)(implicit tx: T): RandomObj[T] =
    format[T].readT(in)

  implicit def format[T <: Exec[T]]: TFormat[T, RandomObj[T]] = anyFmt.cast

  private val anyFmt = new Fmt[Plain]

  private final class Fmt[T <: Exec[T]] extends CastExecFormat[T, RandomObj] {
    def readT(in: DataInput)(implicit tx: T): RandomObj[T] = {
      val id      = tx.readId(in)
      val seedRef = id.readLongVar(in)
      new Impl(id, seedRef)
    }
  }
}

/** A transactional pseudo-random number generator which
 * behaves numerically like `java.util.Random`.
 */
trait RandomObj[T <: Exec[T]] extends Random[T] with Mutable[T] {
  def copy[Out <: Exec[Out]]()(implicit tx: T, txOut: Out): RandomObj[Out]
}