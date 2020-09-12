///*
// *  Durable.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.stm
//
//import de.sciss.lucre.stm
//import de.sciss.lucre.stm.impl.{DurableImpl => Impl}
//import de.sciss.serial.{DataInput, DataOutput, Serializer}
//
//import scala.concurrent.stm.InTxn
//
//object Durable {
//  def apply(factory: DataStore.Factory, mainName: String = "data"): Durable =
//    Impl(factory, mainName = mainName)
//
//  def apply(mainStore: DataStore): Durable = Impl(mainStore = mainStore)
//
//  trait Txn extends DurableLike.Txn[Durable]
//}
//
//object DurableLike {
//  trait Id[S <: DurableLike[S]] extends Identifier[S#Tx] {
//    private[stm] def id: Int
//  }
//
//  trait Txn[S <: DurableLike[S]] extends stm.Txn[S] {
//    def newCachedVar[A](  init: A    )(implicit serializer: Serializer[S#Tx, S#Acc, A]): S#Var[A]
//    def newCachedIntVar(  init: Int  ): S#Var[Int ]
//    def newCachedLongVar( init: Long ): S#Var[Long]
//    def readCachedVar[A]( in: DataInput)(implicit serializer: Serializer[S#Tx, S#Acc, A]): S#Var[A]
//    def readCachedIntVar( in: DataInput): S#Var[Int ]
//    def readCachedLongVar(in: DataInput): S#Var[Long]
//  }
//}
//trait DurableLike[S <: DurableLike[S]] extends Sys[S] with Cursor[S] {
//  final type Var[A]      = stm.Var[S#Tx, A]
//  final type Id          = DurableLike.Id[S]
//  final type Acc         = Unit
//  // final type Entry[A]    = _Var[S#Tx, A]
//  type Tx               <: DurableLike.Txn[S]
//  // type I                <: InMemoryLike[I]
//
//  /** Reports the current number of records stored in the database. */
//  def numRecords(implicit tx: S#Tx): Int
//
//  /** Reports the current number of user records stored in the database.
//    * That is the number of records minus those records used for
//    * database maintenance.
//    */
//  def numUserRecords(implicit tx: S#Tx): Int
//
//  def debugListUserRecords()(implicit tx: S#Tx): Seq[S#Id]
//
//  private[lucre] def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: S#Tx): A
//
//  private[stm] def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: S#Tx): Option[A]
//
//  private[lucre] def write(id: Int )(valueFun: DataOutput => Unit)(implicit tx: S#Tx): Unit
//  private[stm]   def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: S#Tx): Unit
//
//  private[stm]  def remove(id: Int )(implicit tx: S#Tx): Unit
//  private[stm]  def remove(id: Long)(implicit tx: S#Tx): Unit
//
//  private[stm]  def exists(id: Int )(implicit tx: S#Tx): Boolean
//  private[stm]  def exists(id: Long)(implicit tx: S#Tx): Boolean
//
//  private[stm] def store: DataStore
//
//  private[lucre] def newIdValue()(implicit tx: S#Tx): Int
//
//  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): S#Tx  // XXX TODO this might go in Cursor?
//
//  // def inMemory: I
//}
//
//trait Durable extends DurableLike[Durable] {
//  final type Tx = Durable.Txn
//  final type I  = InMemory
//}