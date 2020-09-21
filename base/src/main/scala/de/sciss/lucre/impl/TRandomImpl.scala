/*
 *  RandomImpl.scala
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
package impl

import java.util.concurrent.atomic.AtomicLong

object TRandomImpl {
  private final val multiplier  = 0x5DEECE66DL
  private final val mask        = (1L << 48) - 1
  private final val addend      = 11L

  /** Scrambles a seed value for initializing the underlying long variable.
   * Callers who use the `wrap` method may use this to initially fill the
   * wrapped variable based on a given seed.
   */
  def initialScramble(seed: Long): Long = (seed ^ multiplier) & mask

  def calcSeedUniquifier(): Long = {
    while (true) {
      val current = seedUniquifier.get()
      val next = current * 181783497276652981L
      if (seedUniquifier.compareAndSet(current, next)) return next
    }
    sys.error("Never here")
  }

  private val seedUniquifier = new AtomicLong(8682522807148012L)

//  def apply[T <: Exec[T]](id: Ident[T])(implicit tx: T): TRandom[T] =
//    apply(id, calcSeedUniquifier() ^ System.nanoTime())
//
//  def apply[T <: Exec[T]](id: Ident[T], seed: Long)(implicit tx: T): TRandom[T] =
//    new SysImpl[T](id.newLongVar(initialScramble(seed)))

  def wrap[Tx](peer: TVar[Tx, Long]): TRandom[Tx] = new SysImpl[Tx](peer)

  abstract class BaseImpl[Tx] extends TRandom[Tx] {

    def nextBoolean()(implicit tx: Tx): Boolean = next(1) != 0

    def nextDouble()(implicit tx: Tx): Double =
      ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble

    def nextFloat()(implicit tx: Tx): Float = next(24) / (1 << 24).toFloat

    def nextInt()(implicit tx: Tx): Int = next(32)

    def nextInt(n: Int)(implicit tx: Tx): Int = {
      require(n > 0, "n must be positive")

      if ((n & -n) == n) {
        // n is a power of 2
        return ((n * next(31).toLong) >> 31).toInt
      }

      while (true) {
        val bits = next(31)
        val res = bits % n
        if (bits - res + n >= 1) return res
      }

      sys.error("Never here")
    }

    def nextLong()(implicit tx: Tx): Long = (next(32).toLong << 32) + next(32)

    def setSeed(seed: Long)(implicit tx: Tx): Unit =
      rawSeed = initialScramble(seed)

    private def next(bits: Int)(implicit tx: Tx): Int = {
      val oldSeed   = rawSeed
      val nextSeed  = (oldSeed * multiplier + addend) & mask
      rawSeed       = nextSeed
      (nextSeed >>> (48 - bits)).toInt
    }
  }

  abstract class SysLike[Tx] extends BaseImpl[Tx] {
    protected def seedRef: TVar[Tx, Long]

    final def rawSeed_=(value: Long)(implicit tx: Tx): Unit = seedRef() = value
    final def rawSeed               (implicit tx: Tx): Long = seedRef()
  }

  private final class SysImpl[Tx](protected val seedRef: TVar[Tx, Long]) extends SysLike[Tx]
}
