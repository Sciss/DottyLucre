/*
 *  InMemoryLike.scala
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

import scala.concurrent.stm.{InTxn, Ref => STMRef}

object InMemoryLike {
  trait Id[T <: Txn[T]] extends Ident[T] {
    private[lucre] def id: Int
  }

  trait Txn[T <: Txn[T]] extends lucre.Txn[T] {
    override def system: InMemoryLike[T]

    //    private[stm] def intId(id: Id): Int

    final type Var[A] = InMemoryLike.Var[T, A]
    final type Id     = InMemoryLike.Id[T]

    private[lucre] def getVar[A](vr: InMemoryLike.Var[T, A]): A
    private[lucre] def putVar[A](vr: InMemoryLike.Var[T, A], value: A): Unit
  }

  trait Var[T, A] extends lucre.Var[T, A] {
    private[lucre] def peer: STMRef[A]
  }
}
trait InMemoryLike[Tx <: InMemoryLike.Txn[Tx]] extends Sys /*[S]*/ with Cursor[Tx] {
  final type Id       = InMemoryLike.Id[T]

  type T = Tx // InMemoryLike.Txn[T]

  private[lucre] def attrMap: IdentMap[T, Obj.AttrMap[T]]

  private[lucre] def newIdValue()(implicit tx: T): Int

  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): T
}