/*
 *  IndexMapHandler.scala
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

import de.sciss.lucre.ConstantSerializer
import de.sciss.serial.DataInput

trait IndexMapHandler[T <: Txn[T]] {
  def readIndexMap[A](in: DataInput, tx: T)
                     (implicit index: tx.Acc, serializer: ConstantSerializer[A]): IndexMap[T, A]

  def newIndexMap[A](tx: T, rootTerm: Long, rootValue: A)
                    (implicit index: tx.Acc, serializer: ConstantSerializer[A]): IndexMap[T, A]

  // true is term1 is ancestor of term2
  def isAncestor(term1: Long, term2: Long)(implicit tx: T): Boolean
}