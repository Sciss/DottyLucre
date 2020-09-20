/*
 *  Sys.scala
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

import de.sciss.lucre.{DataStore, DurableLike, InMemoryLike, TSerializer, TSource, TxnLike, confluent, stm, Sys => LSys}
import de.sciss.serial
import de.sciss.serial.DataInput

import scala.collection.immutable.{IndexedSeq => Vec}

/** This is analogous to a `ConfluentLike` trait. Since there is only one system in
  * `LucreConfluent`, it was decided to just name it `confluent.Sys`.
  *
  * @tparam S   the implementing system
  */
trait Sys /*[S <: Sys[S]]*/ extends LSys /*[S]*/ {
  type D <: DurableLike .Txn[D]
  type I <: InMemoryLike.Txn[I]

  type T               <: confluent.Txn[T]
  final type Id         = confluent.Identifier[T]
  final type Acc        = confluent.Access[T]
  final type Var[A]     = confluent.Var[/*T,*/ A]
  // final type Entry[A]   = Sys.Entry[T, A]

//  def durable : D
  // def inMemory: I

  def durableTx (tx: T): D
  //  /* private[lucre] */ def inMemoryTx(tx: Tx): I#Tx

  private[confluent] def fullCache:    CacheMap.Durable[T, Int, DurablePersistentMap[T, Int]]
  // private[confluent] def partialCache: CacheMap.Partial[T, Int, DurablePersistentMap[T, Int]]

  private[confluent] def newIdValue()(implicit tx: T): Int
  private[confluent] def newVersionId(implicit tx: T): Long

  private[confluent] def store: DataStore

  private[confluent] def indexMap: IndexMapHandler[T]

  private[confluent] def flushRegular(meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]])(implicit tx: T): Unit
  private[confluent] def flushRoot   (meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]])(implicit tx: T): Unit

  /* private[confluent] */ def readPath(in: DataInput): Access[T]

  private[confluent] def createTxn(dtx: D, inputAccess: Access[T], retroactive: Boolean, cursorCache: Cache[T],
                                   systemTimeNanos: Long): T

  // ---- cursors ----

  def newCursor()(implicit tx: T): Cursor[T, D]
  /* private[confluent] */ def newCursor (init: Access[T]  )(implicit tx: T): Cursor[T, D]
  /* private[confluent] */ def readCursor(in: DataInput)(implicit tx: T): Cursor[T, D]

  /** Initializes the data structure, by either reading an existing entry or generating the root entry
    * with the `init` function. The method than allows the execution of another function within the
    * same transaction, passing it the data structure root of type `A`. This is typically used to
    * generate access mechanisms, such as extracting a cursor from the data structure, or instantiating
    * a new cursor. The method then returns both the access point to the data structure and the result
    * of the second function.
    *
    * @param init         a function to initialize the data structure (if the database is fresh)
    * @param result       a function to process the data structure
    * @param serializer   a serializer to read or write the data structure
    * @tparam A           type of data structure
    * @tparam B           type of result from the second function. typically this is an `stm.Cursor[S]`
    * @return             the access to the data structure along with the result of the second function.
    */
  def cursorRoot[A, B](init: T => A)(result: T => A => B)
                      (implicit serializer: serial.Serializer[T, Access[T], A]): (Var[A], B)

  /** Initializes the data structure both with a confluently persisted and an ephemeral-durable value.
    *
    * @param confluent    a function that provides the initial confluent data (if the database is fresh)
    * @param durable      a function that provides the initial ephemeral data (if the database is fresh)
    * @param aSer         a serializer to read or write the confluent data structure
    * @param bSer         a serializer to read or write the ephemeral data structure
    * @tparam A           type of confluent data structure
    * @tparam B           type of ephemeral data structure
    * @return             a tuple consisting of a handle to the confluent structure and the
    *                     ephemeral datum. The ephemeral datum, although written to disk, does not
    *                     require an `stm.Source` because `D#Acc` is `Unit` and does not need refresh.
    */
  def rootWithDurable[A, B](confluent: T => A)(durable: D => B)
                           (implicit aSer: TSerializer[T, A],
                                     bSer: TSerializer[D, B]): (TSource[T, A], B)

  /** Retrieves the In information for a given version term. */
  private[confluent] def versionInfo(term: Long)(implicit tx: TxnLike): VersionInfo

  private[confluent] def versionUntil(access: Access[T], timeStamp: Long)(implicit tx: T): Access[T] // XXX TODO: can we get to TxnLike here, too?

  def debugPrintIndex(index: Access[T])(implicit tx: T): String
}