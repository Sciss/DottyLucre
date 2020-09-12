///*
// *  BasicTxnImpl.scala
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
//package impl
//
//import scala.concurrent.stm.{Txn => ScalaTxn, Ref => ScalaRef}
//
//trait BasicTxnImpl[S <: Sys[S]] extends Txn[S] {
//  _: S#Tx =>
//
//  def beforeCommit(fun: S#Tx => Unit): Unit =
//    ScalaTxn.beforeCommit(_ => fun(this))(peer)
//
//  def afterCommit(code: => Unit): Unit =
//    ScalaTxn.afterCommit(_ => code)(peer)
//
//  def newInMemoryMap[K, V]: RefMap[S, K, V] =
//    new SysInMemoryMap[S, K, V]
//
//  def newInMemorySet[A]: RefSet[S, A] =
//    new SysInMemorySet[S, A]
//
//  def newRef[A](init: A): Ref[S#Tx, A] = {
//    val peer = ScalaRef(init)
//    new SysInMemoryRef[S, A](peer)
//  }
//
//  //  @field protected var _context: S#Context = _
////
////  final def use[A](context: S#Context)(fun: => A): A = {
////    val old   = _context
////    _context  = context
////    try {
////      fun
////    } finally {
////      _context = old
////    }
////  }
//}