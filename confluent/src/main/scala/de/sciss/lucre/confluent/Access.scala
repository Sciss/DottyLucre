/*
 *  Access.scala
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

package de.sciss.lucre.confluent

import de.sciss.fingertree.FingerTree
import de.sciss.lucre.TxnLike
import de.sciss.serial.{DataInput, Writable}

object Access {
  def root[T <: Txn[T]]: Access[T] = impl.PathImpl.root[T]
  def info[T <: Txn[T]](access: Access[T])(implicit tx: TxnLike, system: Sys): VersionInfo =
    system.versionInfo(access.term)

  def read[T <: Txn[T]](in: DataInput): Access[T] = impl.PathImpl.read(in)
}

trait Access[T <: Txn[T]] extends Writable with PathLike {
  def ! (implicit tx: T): tx.Acc

  def mkString(prefix: String, sep: String, suffix: String): String

  /** Prepends a single element. */
  def +:(head: Long): Access[T]

  /** Appends a single element. */
  def :+(last: Long): Access[T]

  /** Drops the last element. */
  def index: Access[T]

  /** Drops the head element. */
  def tail:  Access[T]

  def term: Long

  private[confluent] def indexSum: Long

  private[confluent] def apply(idx: Int): Long

  private[confluent] def maxPrefixLength(term: Long): Int

  def seminal: Access[T]

//  private[confluent] def partial: Access[T]

  private[confluent] def tree: FingerTree[(Int, Long), Long] // :-( it's unfortunate having to expose this

  /** Splits off last term, returning index (init) and that last term. */
  def splitIndex: (Access[T], Long)

  // split an index and term at a given point. that is
  // return the `idx` first elements of the path, and the one
  // following (the one found when applying `idx`).
  // although not enforced, `idx` should be an odd number,
  // greater than zero and less than `size`.
  private[confluent] def splitAtIndex(idx: Int): (Access[T], Long)

  private[confluent] def splitAtSum(hash: Long): (Access[T], Long)

  /** Replaces the terminal version with the given `term`.
    * If the new term is on the same tree level as the old term,
    * the term is replaced, otherwise a new tree is entered
    * (the new term is appended twice).
    */
  def addTerm(term: Long)(implicit tx: T): Access[T]

  // drop initial elements
  def drop(num: Int): Access[T]
  def take(num: Int): Access[T]

  /** Retrieves the version information associated with the access path. */
  def info(implicit tx: T): VersionInfo

  /** Truncates the path to a prefix corresponding to the most recent
    * transaction along the path which has occurred not after a given
    * point in (system) time.
    *
    * In other words, calling `info` on the returned path results in
    * a `VersionInfo` object whose `timeStamp` field is less than or
    * equal to the `timeStamp` argument of this method. The only
    * exception is if the `timeStamp` argument is smaller than the
    * root version of system; in that case, the root path is returned
    * instead of an empty path.
    *
    * '''Note:''' This assumes that incremental versions correspond
    * with incremental time stamps. This is not enforced and if this is not the case,
    * the behaviour is undefined. Furthermore, if it is allowed that
    * multiple successive versions have the same time stamp. In that
    * case, it is undefined which of these versions is returned.
    *
    * @param   timeStamp  the query time (in terms of `System.currentTimeMillis`)
    */
  def takeUntil(timeStamp: Long)(implicit tx: T): Access[T]
}