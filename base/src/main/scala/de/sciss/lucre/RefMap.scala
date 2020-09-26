/*
 *  RefMap.scala
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

import scala.collection.immutable

trait RefMap[T <: Exec[T], K, V] {
  def put     (key: K, value: V)(implicit tx: T): Option[V]
  def get     (key: K)(implicit tx: T): Option[V]
  def remove  (key: K)(implicit tx: T): Option[V]
  def contains(key: K)(implicit tx: T): Boolean
  def size            (implicit tx: T): Int
  def isEmpty         (implicit tx: T): Boolean

  def foreach[B](f: ((K, V)) => B)(implicit tx: T): Unit

  def toMap(implicit tx: T): immutable.Map[K, V]
}
