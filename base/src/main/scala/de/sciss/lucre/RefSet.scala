/*
 *  RefSet.scala
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

import scala.collection.immutable

trait RefSet[T <: Exec[T], A] {
  def add     (elem: A)(implicit tx: T): Boolean
  def remove  (elem: A)(implicit tx: T): Boolean
  def contains(elem: A)(implicit tx: T): Boolean
  def size             (implicit tx: T): Int
  def isEmpty          (implicit tx: T): Boolean

  def foreach[B](f: A => B)(implicit tx: T): Unit

  def toSet(implicit tx: T): immutable.Set[A]
}
