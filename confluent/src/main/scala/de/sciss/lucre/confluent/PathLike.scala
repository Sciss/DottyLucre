/*
 *  PathLike.scala
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

package de.sciss.lucre.confluent

trait PathLike {
  def sum: Long

  def size: Int

  def sumUntil(n: Int): Long

  def head: Long
  def last: Long

  def isEmpty : Boolean
  def nonEmpty: Boolean
}
