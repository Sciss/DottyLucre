/*
 *  ReactionMapImpl.scala
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

package de.sciss.lucre.event.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.{Event, Observer, ReactionMap}
import de.sciss.lucre.stm.{IdentMap, Txn, Ident}

import scala.collection.immutable.{Map => IMap}

object ReactionMapImpl {
  def apply[T <: Txn[T]]()(implicit tx: T): ReactionMap[T] = new Impl[T](???) // tx.newInMemoryIdMap)

  private final class Impl[T <: Txn[T]](protected val eventMap: IdentMap[Ident[T], T, IMap[Int, List[Observer[T, _]]]])
    extends Mixin[T] {

    override def toString = s"ReactionMap@${hashCode.toHexString}"
  }

  trait Mixin[T <: Txn[T]] extends ReactionMap[T] {
    protected def eventMap: IdentMap[Ident[T], T, IMap[Int, List[Observer[T, _]]]]

    // self-reference useful when Mixin is added to an event.Sys
    def reactionMap: ReactionMap[T] = this

    final def addEventReaction[A](event: Event[T, A], observer: Observer[T, A])(implicit tx: T): Boolean = {
      val id    = event.node.id
      val slot  = event.slot
      val map0  = eventMap.getOrElse(id  , IMap.empty)
      val list0 = map0    .getOrElse(slot, Nil)
      val list1 = list0 ::: observer :: Nil
      val map1  = map0 + (slot -> list1)
      eventMap.put(id, map1)
      list0.isEmpty
    }

    def removeEventReaction[A](event: Event[T, Any], observer: Observer[T, A])(implicit tx: T): Boolean = {
      val id    = event.node.id
      val slot  = event.slot
      val map0  = eventMap.getOrElse(id  , throw new NoSuchElementException(s"Node $id"))
      val list0 = map0    .getOrElse(slot, throw new NoSuchElementException(s"slot $slot"))
      val list1 = list0.filterNot(_ === observer)
      val map1  = if (list1.isEmpty) map0 - slot else map0 + (slot -> list1)
      if (map1.isEmpty) {
        eventMap.remove(id)
      } else {
        eventMap.put(id, map1)
      }
      list1.isEmpty
    }

    def getEventReactions[A](event: Event[T, A])(implicit tx: T): List[Observer[T, A]] =
      eventMap.getOrElse(event.node.id, IMap.empty).getOrElse(event.slot, Nil)
        .asInstanceOf[List[Observer[T, A]]]

    def hasEventReactions[A](event: Event[T, A])(implicit tx: T): Boolean =
      eventMap.get(event.node.id).exists(_.get(event.slot).isDefined)
  }
}