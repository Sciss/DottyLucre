/*
 *  DurableLike.scala
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

import de.sciss.lucre
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.concurrent.stm.InTxn

object DurableLike {
  trait Id[T <: DurableLike.Txn[T]] extends Ident[T] {
    private[lucre] def id: Int
  }

  trait Txn[T <: Txn[T]] extends lucre.Txn[T] {
    def system: DurableLike[T]

    final type Id     = DurableLike.Id[T]

    def newCachedVar[A](  init: A    )(implicit format: TFormat[T, A]): Var[T, A]
    def newCachedIntVar(  init: Int  ): Var[T, Int ]
    def newCachedLongVar( init: Long ): Var[T, Long]
    def readCachedVar[A]( in: DataInput)(implicit format: TFormat[T, A]): Var[T, A]
    def readCachedIntVar( in: DataInput): Var[T, Int ]
    def readCachedLongVar(in: DataInput): Var[T, Long]
  }
}
trait DurableLike[Tx <: DurableLike.Txn[Tx]] extends Sys /*[S]*/ with Cursor[Tx] {

  final type Id          = DurableLike.Id[T]

//  type Tx               <: DurableLike.Txn[S]
  type T = Tx
  type I <: InMemoryLike.Txn[I]

  /** Reports the current number of records stored in the database. */
  def numRecords(implicit tx: T): Int

  /** Reports the current number of user records stored in the database.
   * That is the number of records minus those records used for
   * database maintenance.
   */
  def numUserRecords(implicit tx: T): Int

  def debugListUserRecords()(implicit tx: T): Seq[Ident[T]]

  private[lucre] def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: T): A

  private[lucre] def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: T): Option[A]

  private[lucre] def write(id: Int )(valueFun: DataOutput => Unit)(implicit tx: T): Unit
  private[lucre] def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: T): Unit

  private[lucre] def remove(id: Int )(implicit tx: T): Unit
  private[lucre] def remove(id: Long)(implicit tx: T): Unit

  private[lucre] def exists(id: Int )(implicit tx: T): Boolean
  private[lucre] def exists(id: Long)(implicit tx: T): Boolean

  private[lucre] def store: DataStore

  private[lucre] def newIdValue()(implicit tx: T): Int

  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): T  // XXX TODO this might go in Cursor?

  def inMemory: InMemoryLike[I]
}
