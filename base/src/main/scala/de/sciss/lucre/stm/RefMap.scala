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

package de.sciss.lucre.stm

import scala.collection.immutable

trait RefMap[Tx, K, V] {
  def put     (key: K, value: V)(implicit tx: Tx): Option[V]
  def get     (key: K)(implicit tx: Tx): Option[V]
  def remove  (key: K)(implicit tx: Tx): Option[V]
  def contains(key: K)(implicit tx: Tx): Boolean
  def size            (implicit tx: Tx): Int
  def isEmpty         (implicit tx: Tx): Boolean

  def foreach[B](f: ((K, V)) => B)(implicit tx: Tx): Unit

  def toMap(implicit tx: Tx): immutable.Map[K, V]
}
