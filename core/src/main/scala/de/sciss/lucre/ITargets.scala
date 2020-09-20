/*
 *  ITargets.scala
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

package de.sciss.lucre

import scala.annotation.tailrec
import scala.concurrent.stm.TMap

object ITargets {
  def apply[T <: Txn[T]]: ITargets[T] = new Impl[T]

  private final class Impl[T <: Txn[T]] extends ITargets[T] {
    import TxnLike.peer

    private[this] val connections = TMap.empty[IEvent[T, Any], List[IEvent[T, Any]]]
    private[this] val reactions   = TMap.empty[IEvent[T, Any], List[Observer[T, _]]]

    def children(parent: IEvent[T, Any])(implicit tx: T): List[IEvent[T, Any]] =
      connections.get(parent).getOrElse(Nil)

    def add(parent: IEvent[T, Any], sel: IEvent[T, Any])(implicit tx: T): Boolean = {
      val before    = children(parent)
      val wasEmpty  = before.isEmpty
      val now       = if (wasEmpty) sel :: Nil else before :+ sel
      connections  += parent -> now
      wasEmpty
    }

    @tailrec
    private def listRemove[A](item: A, rem: List[A], res: List[A]): List[A] = rem match {
      case `item` :: tail => res.reverse ::: tail
      case Nil            => res.reverse
      case head :: tail   => listRemove(item, tail, head :: res)
    }

    def remove(parent: IEvent[T, Any], sel: IEvent[T, Any])(implicit tx: T): Boolean = {
      val before    = children(parent)
      val now       = listRemove(sel, before, Nil)
      val isEmpty   = now.isEmpty

      if (isEmpty)
        connections -= parent
      else
        connections += parent -> now

      isEmpty
    }

    def addEventReaction[A](event: IEvent[T, A], observer: Observer[T, A])(implicit tx: T): Boolean = {
      val before    = getEventReactions(event)
      val wasEmpty  = before.isEmpty
      val now       = if (wasEmpty) observer :: Nil else before :+ observer
      reactions    += event -> now
      wasEmpty
    }

    def removeEventReaction[A](event: IEvent[T, Any], observer: Observer[T, A])(implicit tx: T): Boolean = {
      val before    = getEventReactions(event)
      val now       = listRemove(observer, before, Nil)
      val isEmpty   = now.isEmpty

      if (isEmpty)
        reactions += event -> now
      else
        reactions -= event

      isEmpty
    }

    def getEventReactions[A](event: IEvent[T, A])(implicit tx: T): List[Observer[T, A]] =
      reactions.get(event) match {
        case Some(xs) => xs.asInstanceOf[List[Observer[T, A]]]
        case None     => Nil
      }

    def hasEventReactions[A](event: IEvent[T, A])(implicit tx: T): Boolean =
      reactions.contains(event)
  }
}

/** Interconnection management for in-memory events.
  * A centralized instance that combines the functionality of `Targets` with `ReactionMap`.
  */
trait ITargets[T <: Exec[T]] {
  def children(parent: IEvent[T, Any])(implicit tx: T): List[IEvent[T, Any]]

  /** Adds a dependant to this node target.
    *
    * @param parent the parent event to be pushing to the dependant
    * @param sel  the target selector to which an event will be pushed
    *
    * @return  `true` if this was the first dependant registered with the given parent, `false` otherwise
    */
  def add(parent: IEvent[T, Any], sel: IEvent[T, Any])(implicit tx: T): Boolean

  /** Removes a dependant from this node target.
    *
    * @param parent the parent event which is currently pushing to the dependant
    * @param sel  the target selector which was registered with the parent
    *
    * @return  `true` if this was the last dependant unregistered with the given parent, `false` otherwise
    */
  def remove(parent: IEvent[T, Any], sel: IEvent[T, Any])(implicit tx: T): Boolean

  // ---- reaction map ----

  def addEventReaction    [A](event: IEvent[T, A  ], observer: Observer[T, A])(implicit tx: T): Boolean
  def removeEventReaction [A](event: IEvent[T, Any], observer: Observer[T, A])(implicit tx: T): Boolean
  def getEventReactions   [A](event: IEvent[T, A])(implicit tx: T): List[Observer[T, A]]
  def hasEventReactions   [A](event: IEvent[T, A])(implicit tx: T): Boolean
}
