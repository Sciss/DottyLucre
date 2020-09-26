/*
 *  SysInMemorySet.scala
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

import scala.concurrent.stm.TSet

final class SysInMemorySet[T <: Txn[T], A] extends RefSet[T, A] {
  private[this] val peer = TSet.empty[A]

  def add     (elem: A)(implicit tx: T): Boolean = peer.add     (elem)(tx.peer)
  def remove  (elem: A)(implicit tx: T): Boolean = peer.remove  (elem)(tx.peer)
  def contains(elem: A)(implicit tx: T): Boolean = peer.contains(elem)(tx.peer)
  def size             (implicit tx: T): Int     = peer.size          (tx.peer)
  def isEmpty          (implicit tx: T): Boolean = peer.isEmpty       (tx.peer)

  def foreach[B](f: A => B)(implicit tx: T): Unit = peer.foreach(f)(tx.peer)

  def toSet(implicit tx: T): Set[A] = peer.snapshot
}
