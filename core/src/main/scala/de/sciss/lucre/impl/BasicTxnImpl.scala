/*
 *  BasicTxnImpl.scala
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
package impl

import scala.concurrent.stm.{Txn => ScalaTxn, Ref => ScalaRef}

trait BasicTxnImpl[T <: Txn[T]] extends Txn[T] {
  self: T =>

  def beforeCommit(fun: T => Unit): Unit =
    ScalaTxn.beforeCommit(_ => fun(this))(peer)

  def afterCommit(code: => Unit): Unit =
    ScalaTxn.afterCommit(_ => code)(peer)

  def newInMemoryMap[K, V]: RefMap[T, K, V] =
    new SysInMemoryMap[T, K, V]

  def newInMemorySet[A]: RefSet[T, A] =
    new SysInMemorySet[T, A]

  def newRef[A](init: A): Ref[T, A] = {
    val peer = ScalaRef(init)
    new SysInMemoryRef[A](peer)
  }

  //  @field protected var _context: S#Context = _
  //
  //  final def use[A](context: S#Context)(fun: => A): A = {
  //    val old   = _context
  //    _context  = context
  //    try {
  //      fun
  //    } finally {
  //      _context = old
  //    }
  //  }
}