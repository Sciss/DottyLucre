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
//  trait Id[S <: Sys[S]] extends Identifier[S#Tx] {
//    private[stm] def id: Int
//  }
//
//  trait Txn[S <: Sys[S]] extends stm.Txn[S] {
//    private[stm] def getVar[A](vr: S#Var[A]): A
//    private[stm] def putVar[A](vr: S#Var[A], value: A): Unit
//  }
//
//  trait Var[S <: Sys[S], A] extends stm.Var[S#Tx, A] {
//    private[stm] def peer: STMRef[A]
//  }
//}
//trait InMemoryLike[S <: InMemoryLike[S]] extends Sys[S] with Cursor[S] {
//  final type Var[A]   = InMemoryLike.Var[S, A]
//  final type Id       = InMemoryLike.Id[S]
//  final type Acc      = Unit
//
//  type Tx <: InMemoryLike.Txn[S]
//
//  private[lucre] def attrMap: IdentifierMap[S#Id, S#Tx, Obj.AttrMap[S]]
//
//  private[lucre] def newIdValue()(implicit tx: S#Tx): Int
//
//  def wrap(peer: InTxn, systemTimeNanos: Long = 0L) : S#Tx
//}