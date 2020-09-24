/*
 *  IdentMapImpl.scala
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

import scala.concurrent.stm.TMap

object IdentMapImpl {
  def apply[T <: Txn[T], A](intView: T => Ident[T] => Int): IdentMap[T, A] =
    new InMemoryInt[T, A](intView)

  private final class InMemoryInt[T <: Txn[T], A](intView: T => Ident[T] => Int)
    extends IdentMap[T, A] {

    private[this] val peer = TMap.empty[Int, A]

    def get(id: Ident[T])(implicit tx: T): Option[A] = peer.get(intView(tx)(id))(tx.peer)

    def getOrElse(id: Ident[T], default: => A)(implicit tx: T): A = get(id).getOrElse(default)

    def put(id: Ident[T], value: A)(implicit tx: T): Unit =
      peer.put(intView(tx)(id), value)(tx.peer)

    def contains(id: Ident[T])(implicit tx: T): Boolean = peer.contains(intView(tx)(id))(tx.peer)

    def remove(id: Ident[T])(implicit tx: T): Unit =
      peer.remove(intView(tx)(id))(tx.peer)

    override def toString = s"IdentifierMap@${hashCode.toHexString}"

    // def write(out: DataOutput): Unit = ()

    def dispose()(implicit tx: T): Unit = ()
  }
}