/*
 *  SysInMemoryMap.scala
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

package de.sciss.lucre
package impl

import scala.concurrent.stm.TMap

final class SysInMemoryMap[T <: Txn[T], K, V] extends RefMap[T, K, V] {
  private[this] val peer = TMap.empty[K, V]

  def put     (key: K, value: V )(implicit tx: T): Option[V]  = peer.put      (key, value) (tx.peer)
  def remove  (key: K           )(implicit tx: T): Option[V]  = peer.remove   (key)        (tx.peer)
  def contains(key: K           )(implicit tx: T): Boolean    = peer.contains (key)        (tx.peer)
  def get     (key: K           )(implicit tx: T): Option[V]  = peer.get      (key)        (tx.peer)
  def size                       (implicit tx: T): Int        = peer.size                  (tx.peer)
  def isEmpty                    (implicit tx: T): Boolean    = peer.isEmpty               (tx.peer)

  def foreach[B](f: ((K, V)) => B)(implicit tx: T): Unit = peer.foreach[B](f)(tx.peer)

  def toMap(implicit tx: T): Map[K, V] = peer.snapshot
}
