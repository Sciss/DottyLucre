/*
 *  Push.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.experiment.Log.logEvent

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{Map => IMap}

object Push {
  private[experiment] def apply[T <: Txn[T], A](origin: Event[T, A], update: A)(implicit tx: T): Unit = {
    val push = new Impl(origin, update)
    logEvent("push begin")
    push.visitChildren(origin)
    logEvent("pull begin")
    push.pull()
    logEvent("pull end")
  }

  type Parents[T <: Txn[T]] = Set[Event[T, Any]]

  private def NoParents[T <: Txn[T]]: Parents[T] = Set.empty[Event[T, Any]]

  // private type Visited[T <: Txn[T]] = IMap[Event[T, Any], Parents[T]]
  private final class Reaction[T <: Txn[T], +A](update: A, observers: List[Observer[T, A]]) {
    def apply()(implicit tx: T): Unit =
      observers.foreach(_.apply(update))
  }

  private final class Impl[T <: Txn[T]](origin: Event[T, Any], val update: Any)(implicit tx: T)
    extends Pull[T] {

    private[this] var pushMap   = IMap(origin -> NoParents[T])
    private[this] var pullMap   = IMap.empty[EventLike[T, Any], Option[Any]]

    private[this] var indent    = ""

    @elidable(CONFIG) private[this] def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private[this] def decIndent(): Unit = indent = indent.substring(2)

    private[this] def addVisited(child: Event[T, Any], parent: Event[T, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      logEvent(s"${indent}visit $child  (new ? ${parents.isEmpty})")
      pushMap += ((child, parents + parent))
      parents.isEmpty
    }

    def visitChildren(parent: Event[T, Any]): Unit = {
      val inlet = parent.slot
      incIndent()
      try {
        val childEvents = parent.node._targets.children
        childEvents.foreach { case (inlet2, child) =>
          if (inlet2.toInt === inlet) {
            visit(child, parent)
          }
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: Event[T, Any], parent: Event[T, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    // cf. https://stackoverflow.com/questions/32098481
    def contains(source: EventLike[T, Any]): Boolean = source match {
      case e: Event[T, Any] => pushMap.contains(e)
      case _ => false
    }

    def isOrigin(that  : EventLike[T, Any]): Boolean    = that === origin
    def parents (child : Event    [T, Any]): Parents[T] = pushMap.getOrElse(child, NoParents)

    def pull(): Unit = {
      val reactions: List[Reaction[T, Any]] = pushMap.iterator.flatMap { case (event, _) =>
        val observers = tx.reactionMap.getEventReactions(event)
        if (observers.nonEmpty || event.isInstanceOf[Caching])
          apply[Any](event).map(new Reaction(_, observers)) else None
      } .toList
      logEvent(s"numReactions = ${reactions.size}")
      reactions.foreach(_.apply())
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    // caches pulled values
    def apply[A](source: EventLike[T, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) =>
            logEvent(s"${indent}pull $source  (new ? false)")
            res.asInstanceOf[Option[A]]
          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val res = source.pullUpdate(this)
            pullMap += ((source, res))
            res
        }
      } finally {
        decIndent()
      }
    }
  }
}

trait Pull[T <: Txn[T]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: Event[T, Any]): Push.Parents[T]

  /** Pulls the update from the given source. */
  def apply[A](source: EventLike[T, A]): Option[A]

  /** Whether the selector has been visited during the push phase. */
  def contains(source: EventLike[T, Any]): Boolean

  def isOrigin(source: EventLike[T, Any]): Boolean
}