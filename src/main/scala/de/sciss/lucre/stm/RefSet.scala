/*
 *  RefSet.scala
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

trait RefSet[S <: Base[S], A] {
  def add     (elem: A)(implicit tx: S#Tx): Boolean
  def remove  (elem: A)(implicit tx: S#Tx): Boolean
  def contains(elem: A)(implicit tx: S#Tx): Boolean
  def size             (implicit tx: S#Tx): Int
  def isEmpty          (implicit tx: S#Tx): Boolean

  def foreach[B](f: A => B)(implicit tx: S#Tx): Unit

  def toSet(implicit tx: S#Tx): immutable.Set[A]
}
