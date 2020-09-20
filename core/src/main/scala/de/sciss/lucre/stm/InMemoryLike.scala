///*
// *  InMemoryLike.scala
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
//
//import scala.concurrent.stm.{Ref => STMRef, InTxn}
//
//object InMemoryLike {
//  trait Id[T <: Txn[T]] extends Ident[T] {
//    private[stm] def id: Int
//  }
//
//  trait Txn[T <: Txn[T]] extends stm.Txn[T] {
//    protected def system: InMemoryLike[T]
//    
////    private[stm] def intId(id: Id): Int
//
//    final type Var[A] = InMemoryLike.Var[T, A]
//    final type Acc    = Unit
//
//    private[stm] def getVar[A](vr: InMemoryLike.Var[T, A]): A
//    private[stm] def putVar[A](vr: InMemoryLike.Var[T, A], value: A): Unit
//  }
//
//  trait Var[T <: stm.Txn[T], A] extends stm.Var[T, A] {
//    private[stm] def peer: STMRef[A]
//  }
//}
//trait InMemoryLike[Tx <: Txn[Tx]] extends Sys /*[S]*/ with Cursor[Tx] {
////  final type Var[A]   = InMemoryLike.Var[S, A]
////  final type Id       = InMemoryLike.Id[S]
////  final type Acc      = Unit
//
//  type T = Tx // InMemoryLike.Txn[T]
//
//  private[lucre] def attrMap: IdentMap[Ident[T], T, Obj.AttrMap[T]]
//
//  private[lucre] def newIdValue()(implicit tx: T): Int
//
//  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): T
//}