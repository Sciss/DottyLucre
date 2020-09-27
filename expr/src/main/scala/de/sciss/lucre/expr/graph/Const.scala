/*
 *  Const.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.impl.IDummyEvent
import de.sciss.lucre.{Exec, IChangeEvent, IExpr, Txn}

object Const {
  private[sciss] final class Expanded[T <: Exec[T], A](peer: A)
    extends IExpr[T, A] {

    override def toString: String = peer.toString

    def changed: IChangeEvent[T, A] = IDummyEvent.applyChange

    def value(implicit tx: T): A = peer

    def dispose()(implicit tx: T): Unit = ()
  }
}
final case class Const[A](value: A) extends Ex[A] {
  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new Const.Expanded[T, A](value)

  override def toString: String = value.toString
}