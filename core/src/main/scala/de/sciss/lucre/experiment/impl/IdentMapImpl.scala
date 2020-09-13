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

package de.sciss.lucre.experiment.impl

import de.sciss.lucre.experiment.{IdentMap, TxnLike}

import scala.concurrent.stm.TMap

object IdentMapImpl {
  def apply[Id, Tx <: TxnLike, A](intView: Tx => Id => Int): IdentMap[Id, Tx, A] =
    new InMemoryInt[Id, Tx, A](intView)

  private final class InMemoryInt[Id, Tx <: TxnLike, A](intView: Tx => Id => Int)
    extends IdentMap[Id, Tx, A] {

    private[this] val peer = TMap.empty[Int, A]

    def get(id: Id)(implicit tx: Tx): Option[A] = peer.get(intView(tx)(id))(tx.peer)

    def getOrElse(id: Id, default: => A)(implicit tx: Tx): A = get(id).getOrElse(default)

    def put(id: Id, value: A)(implicit tx: Tx): Unit =
      peer.put(intView(tx)(id), value)(tx.peer)

    def contains(id: Id)(implicit tx: Tx): Boolean = peer.contains(intView(tx)(id))(tx.peer)

    def remove(id: Id)(implicit tx: Tx): Unit =
      peer.remove(intView(tx)(id))(tx.peer)

    override def toString = s"IdentifierMap@${hashCode.toHexString}"

    // def write(out: DataOutput): Unit = ()

    def dispose()(implicit tx: Tx): Unit = ()
  }
}