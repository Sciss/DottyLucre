/*
 *  TxnRandom.scala
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

/** Like java's random, but within a transactional cell. */
object TRandom {
//  def apply[T <: Exec[T]](id: S#Id)(implicit tx: T): TRandom[T] = Impl(id)
//
//  def apply[T <: Exec[T]](id: S#Id, seed: Long)(implicit tx: T): TRandom[T] = Impl(id, seed)
//
//  def wrap[Tx](peer: TVar[Tx, Long]): TRandom[Tx] = Impl.wrap(peer)
}

/** A transactional pseudo-random number generator which
 * behaves numerically like `java.util.Random`.
 */
trait TRandom[-T] {
  /** Generates a random `Boolean` value. */
  def nextBoolean  ()(implicit tx: T): Boolean

  /** Generates a random `Double` value, uniformly distributed
   * between `0.0` (inclusive) and `1.0` (exclusive).
   */
  def nextDouble   ()(implicit tx: T): Double

  /** Generates a random `Float` value, uniformly distributed
   * between `0.0f` (inclusive) and `1.0f` (exclusive).
   */
  def nextFloat    ()(implicit tx: T): Float

  /** Generates a random `Int` value in the range `Int.MinValue` to `Int.MaxValue`. */
  def nextInt      ()(implicit tx: T): Int

  /** Generates a random `Int` value in the range of 0 (inclusive) until the specified value `n` (exclusive). */
  def nextInt(n: Int)(implicit tx: T): Int

  /** Generates a random `Long` value in the range `Long.MinValue` to `Long.MaxValue`.
   *
   * __WARNING:__
   * Because it uses the same algorithm as `java.util.Random`, with a seed of only 48 bits,
   * this function will not return all possible long values!
   */
  def nextLong     ()(implicit tx: T): Long

  /** Resets the internal seed value to the given argument. */
  def setSeed(seed: Long)(implicit tx: T): Unit

  /** Resets the internal seed value to the given argument. This is a raw seed value
   * as obtained from `getRawSeed`. For user operation, use `setSeed` instead,
   * which further scrambles the seed value.
   */
  def rawSeed_=(seed: Long)(implicit tx: T): Unit

  def rawSeed(implicit tx: T): Long
}