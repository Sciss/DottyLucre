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

package de.sciss.lucre.experiment

import de.sciss.lucre.experiment

import scala.concurrent.stm.{Ref => STMRef, InTxn}

object InMemoryLike {
  trait Id[T <: Txn[T]] extends Ident[T] {
    private[experiment] def id: Int
  }

  trait Txn[T <: Txn[T]] extends experiment.Txn[T] {
    protected def system: InMemoryLike[T]

    //    private[stm] def intId(id: Id): Int

    final type Var[A] = InMemoryLike.Var[/*T,*/ A]
    final type Acc    = Unit
    final type Id     = InMemoryLike.Id[T]

    private[experiment] def getVar[A](vr: InMemoryLike.Var[/*T,*/ A]): A
    private[experiment] def putVar[A](vr: InMemoryLike.Var[/*T,*/ A], value: A): Unit
  }

  trait Var[/*T <: experiment.Txn[T],*/ A] extends experiment.Var[/*T,*/ A] {
    private[experiment] def peer: STMRef[A]
  }
}
trait InMemoryLike[Tx <: InMemoryLike.Txn[Tx]] extends Sys /*[S]*/ with Cursor[Tx] {
  //  final type Var[A]   = InMemoryLike.Var[S, A]
  final type Id       = InMemoryLike.Id[T]
  //  final type Acc      = Unit

  type T = Tx // InMemoryLike.Txn[T]

  private[lucre] def attrMap: IdentMap[Id, T, Obj.AttrMap[T]]

  private[lucre] def newIdValue()(implicit tx: T): Int

  def wrap(peer: InTxn, systemTimeNanos: Long = 0L): T
}