/*
 *  MeldInfo.scala
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

import de.sciss.lucre.Confluent

object MeldInfo {
  def empty[T <: Txn[T]]: MeldInfo[T] = anyMeldInfo.asInstanceOf[MeldInfo[T]]

  private val anyMeldInfo = MeldInfo[Confluent.Txn](-1, Set.empty)
}

final case class MeldInfo[T <: Txn[T]](highestLevel: Int, highestTrees: Set[Access[T]]) {
  def requiresNewTree: Boolean = highestTrees.size > 1

  def outputLevel: Int = if (requiresNewTree) highestLevel + 1 else highestLevel

  /** An input tree is relevant if its level is higher than the currently observed
    * highest level, or if it has the same level but was not recorded in the set
    * of highest trees.
    */
  def isRelevant(level: Int, seminal: Access[T]): Boolean =
    level > highestLevel || level == highestLevel && !highestTrees.contains(seminal)

  def add(level: Int, seminal: Access[T]): MeldInfo[T] =
    if (isRelevant(level, seminal)) MeldInfo[T](level, highestTrees + seminal) else this

  def isEmpty: Boolean = highestLevel < 0
}