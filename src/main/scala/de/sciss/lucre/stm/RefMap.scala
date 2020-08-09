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

trait RefMap[S <: Base[S], K, V] {
  def put     (key: K, value: V)(implicit tx: S#Tx): Option[V]
  def get     (key: K)(implicit tx: S#Tx): Option[V]
  def remove  (key: K)(implicit tx: S#Tx): Option[V]
  def contains(key: K)(implicit tx: S#Tx): Boolean
  def size            (implicit tx: S#Tx): Int
  def isEmpty         (implicit tx: S#Tx): Boolean

  def foreach[B](f: ((K, V)) => B)(implicit tx: S#Tx): Unit

  def toMap(implicit tx: S#Tx): immutable.Map[K, V]
}
