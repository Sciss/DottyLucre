/*
 *  IndexMap.scala
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

import de.sciss.serial.Writable

trait IndexMap[-T, A] extends Writable {
  def add(term: Long, value: A)(implicit tx: T): Unit

  def nearest      (term: Long)(implicit tx: T): (Long, A)
  def nearestOption(term: Long)(implicit tx: T): Option[(Long, A)]

  def nearestUntil(timeStamp: Long, term: Long)(implicit tx: T): Option[(Long, A)]

  def debugPrint(implicit tx: T): String
}