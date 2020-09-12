///*
// *  SysInMemoryRef.scala
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
//import de.sciss.serial.DataOutput
//
//import scala.concurrent.stm.{Ref => ScalaRef}
//
//final class SysInMemoryRef[S <: Sys[S], A](val peer: ScalaRef[A])
//  extends InMemoryLike.Var[S, A] {
//
//  override def toString = s"Var<${hashCode().toHexString}>"
//
//  def apply()     (implicit tx: S#Tx): A    = peer.get    (tx.peer)
//  def update(v: A)(implicit tx: S#Tx): Unit = peer.set (v)(tx.peer)
//  def swap  (v: A)(implicit tx: S#Tx): A    = peer.swap(v)(tx.peer)
//
//  def write(out: DataOutput): Unit = ()
//
//  def dispose()(implicit tx: S#Tx): Unit =
//    peer.set(null.asInstanceOf[A])(tx.peer)
//}
