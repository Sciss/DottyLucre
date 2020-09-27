/*
 *  PlainInMemorySet.scala
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

import scala.collection.mutable

final class PlainInMemorySet[A] extends RefSet[Plain, A] {
  private[this] val peer = mutable.Set.empty[A]

  def add     (elem: A)(implicit tx: Plain): Boolean = peer.add     (elem)
  def remove  (elem: A)(implicit tx: Plain): Boolean = peer.remove  (elem)
  def contains(elem: A)(implicit tx: Plain): Boolean = peer.contains(elem)
  def size             (implicit tx: Plain): Int     = peer.size
  def isEmpty          (implicit tx: Plain): Boolean = peer.isEmpty

  def foreach[B](f: A => B)(implicit tx: Plain): Unit = peer.foreach(f)

  def toSet(implicit tx: Plain): Set[A] = peer.toSet[A]
}
