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

package de.sciss.lucre.experiment

import de.sciss.lucre.experiment
import de.sciss.serial.{DataInput, DataOutput}

import scala.concurrent.stm.InTxn

object DurableLike {
  trait Id[T <: DurableLike.Txn[T]] extends Ident[T] {
    private[experiment] def id: Int
  }

  trait Txn[T <: Txn[T]] extends experiment.Txn[T] {
    def system: DurableLike[T]

//    final type Var[A] = DurableLike.Var[/*T,*/ A]
    final type Acc    = Unit
    final type Id     = DurableLike.Id[T]

//    def newCachedVar[A](  init: A    )(implicit serializer: TSerializer[T, A]): Var[A]
//    def newCachedIntVar(  init: Int  ): Var[Int ]
//    def newCachedLongVar( init: Long ): Var[Long]
//    def readCachedVar[A]( in: DataInput)(implicit serializer: TSerializer[T, A]): Var[A]
//    def readCachedIntVar( in: DataInput): Var[Int ]
//    def readCachedLongVar(in: DataInput): Var[Long]
    
  }
}
trait DurableLike[T <: DurableLike.Txn[T]] extends Sys /*[S]*/ with Cursor[T] {
//  final type Var[A]      = experiment.Var[A]
  
  final type Id          = DurableLike.Id[T]

  //  final type Acc         = Unit
  // final type Entry[A]    = _Var[S#Tx, A]
//  type Tx               <: DurableLike.Txn[S]
//  type Tx = T
  // type I                <: InMemoryLike[I]

  /** Reports the current number of records stored in the database. */
  def numRecords(implicit tx: T): Int

  /** Reports the current number of user records stored in the database.
   * That is the number of records minus those records used for
   * database maintenance.
   */
  def numUserRecords(implicit tx: T): Int

  def debugListUserRecords()(implicit tx: T): Seq[Ident[T]]

  private[experiment] def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: T): A

  private[experiment] def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: T): Option[A]

  private[experiment] def write(id: Int )(valueFun: DataOutput => Unit)(implicit tx: T): Unit
  private[experiment] def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: T): Unit

  private[experiment] def remove(id: Int )(implicit tx: T): Unit
  private[experiment] def remove(id: Long)(implicit tx: T): Unit

  private[experiment] def exists(id: Int )(implicit tx: T): Boolean
  private[experiment] def exists(id: Long)(implicit tx: T): Boolean

  private[experiment] def store: DataStore

  private[experiment] def newIdValue()(implicit tx: T): Int

  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): T  // XXX TODO this might go in Cursor?

  // def inMemory: I
}
