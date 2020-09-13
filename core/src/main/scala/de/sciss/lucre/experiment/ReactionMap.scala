/*
 *  ReactionMap.scala
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

package de.sciss.lucre.experiment

import de.sciss.lucre.experiment.impl.ReactionMapImpl

object ReactionMap {
  def apply[T <: Txn[T]]()(implicit tx: T): ReactionMap[T] = ReactionMapImpl[T]()
}

trait ReactionMap[T <: Txn[T]] {
  def addEventReaction   [A](event: Event[T, A  ], observer: Observer[T, A])(implicit tx: T): Boolean
  def removeEventReaction[A](event: Event[T, Any], observer: Observer[T, A])(implicit tx: T): Boolean

  def getEventReactions[A](event: Event[T, A])(implicit tx: T): List[Observer[T, A]]
  def hasEventReactions[A](event: Event[T, A])(implicit tx: T): Boolean
}