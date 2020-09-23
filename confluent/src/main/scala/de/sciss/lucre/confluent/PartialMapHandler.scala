/*
 *  PartialMapHandler.scala
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

import de.sciss.serial.{ConstFormat, DataInput}

trait PartialMapHandler[-T] {
  def getIndexTreeTerm(term: Long)(implicit tx: T): Long

  def readPartialMap[A](in: DataInput)
                       (implicit tx: T, format: ConstFormat[A]): IndexMap[T, A]

  def newPartialMap[A](rootValue: A)
                      (implicit tx: T, format: ConstFormat[A]): IndexMap[T, A]
}