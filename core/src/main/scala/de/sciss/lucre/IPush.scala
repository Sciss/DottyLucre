/*
 *  IPush.scala
 *  (Lucre 4)
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.IPull.Phase
import de.sciss.lucre.Log.logEvent
import de.sciss.model.Change

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{Map => IMap}

object IPush {
  private[lucre] def apply[T <: Exec[T], A](origin: IEvent[T, A], update: A)
                                           (implicit tx: T, targets: ITargets[T]): Unit = {
    val push = new Impl(origin, update)
    logEvent("ipush begin")
    push.visitChildren(origin)
    logEvent("ipull begin")
    push.pull()
    logEvent("ipull end")
  }

  type Parents[T <: Exec[T]] = Set[IEvent[T, Any]]

  private def NoParents[T <: Exec[T]]: Parents[T] = Set.empty[IEvent[T, Any]]

  // private type Visited[S <: Sys[T]] = IMap[Event[T, Any], Parents[T]]
  private final class Reaction[T <: Exec[T], +A](update: A, observers: List[Observer[T, A]]) {
    def apply()(implicit tx: T): Unit =
      observers.foreach(_.apply(update))
  }
  
  private final class Value {
    var state = 0 // 1 = has before, 2 = has now, 4 = has full
    var full  : Option[Any] = None
    var before: Any = _
    var now   : Any = _
    
    def hasBefore : Boolean = (state & 1) != 0
    def hasNow    : Boolean = (state & 2) != 0
    def hasFull   : Boolean = (state & 4) != 0

    def hasPhase(implicit phase: Phase): Boolean =
      if (phase.isNow) hasNow else hasBefore

    def resolvePhase[A](implicit phase: Phase): A =
      if (phase.isNow) now.asInstanceOf[A] else before.asInstanceOf[A]

    def resolveFull[A](implicit phase: Phase): A = {
      val ch = full.get.asInstanceOf[Change[A]]
      if (phase.isNow) ch.now else ch.before
    }

    def setBefore(v: Any): Unit = {
      before = v
      state |= 1
      if (!hasFull && hasNow) createFull()
    }

    def setNow(v: Any): Unit = {
      now = v
      state |= 2
      if (!hasFull && hasBefore) createFull()
    }
    
    def setFull(v: Option[Any]): Unit = {
      full = v
      state |= 4
      if (!hasNow || !hasBefore) v match {
        case Some(Change(_before, _now)) =>
          before  = _before
          now     = _now
          state  |= 3

        case _ =>
      }
    }
    
    def createFull(): Unit = {
      full = if (before == now) None else Some(Change(before, now))
      state |= 4
    }
  }

  private final class Impl[T <: Exec[T]](origin: IEvent[T, Any], val update: Any)
                                        (implicit tx: T, targets: ITargets[T])
    extends IPull[T] {

    private[this] var pushMap   = IMap(origin -> NoParents[T])
    private[this] var pullMap   = IMap.empty[IEvent[T, Any], Value]

    private[this] var indent    = ""

    @elidable(CONFIG) private[this] def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private[this] def decIndent(): Unit = indent = indent.substring(2)

    private[this] def addVisited(child: IEvent[T, Any], parent: IEvent[T, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      logEvent(s"${indent}visit $child  (new ? ${parents.isEmpty})")
      pushMap += ((child, parents + parent))
      parents.isEmpty
    }

    def visitChildren(parent: IEvent[T, Any]): Unit = {
//      val inlet = parent.slot
      incIndent()
      try {
        val childEvents = targets.children(parent) // parent.node._targets.children
        childEvents.foreach { /* case (inlet2, */ child /* ) */ =>
//          if (inlet2 === inlet) {
            visit(child, parent)
//          }
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: IEvent[T, Any], parent: IEvent[T, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    def contains(source: IEvent[T, Any]): Boolean     = pushMap.contains(source)

    def isOrigin(that  : IEvent[T, Any]): Boolean     = that === origin
    def parents (child : IEvent[T, Any]): Parents[T]  = pushMap.getOrElse(child, NoParents)

    def pull(): Unit = {
      val m0    = currentPull.get()
      val isNew = !m0.contains(tx)
      if (isNew) currentPull.set(m0 + (tx -> this))
      try {
        val reactions: List[Reaction[T, Any]] = pushMap.iterator.flatMap { case (event, _) =>
          val observers: List[Observer[T, Any]] = targets.getEventReactions(event) // tx.reactionMap.getEventReactions(event)
          val hasObs = observers.nonEmpty
          if (hasObs || event.isInstanceOf[Caching]) {
            val opt = apply[Any](event)
            if (hasObs) opt.map(new Reaction(_, observers)) else None
          } else None
        } .toList
        logEvent(s"numReactions = ${reactions.size}")
        reactions.foreach(_.apply())
      } finally {
        if (isNew) currentPull.set(m0)
      }
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    def resolveChange[A](implicit phase: IPull.Phase): A = {
      logEvent(s"${indent}resolveChange")
      val ch = update.asInstanceOf[Change[A]]
      if (phase.isNow) ch.now else ch.before
    }

    // this is simply to avoid type mismatch for `A`
    def resolveExpr[A](in: IExpr[T, A])(implicit phase: IPull.Phase): A =
      resolveChange

    // Caches pulled values.
    // Noe that we do not check `nonCachedTerms`, implying that
    // `It.Expanded` forbids the use of `pullUpdate` (it throws
    // an exception), and therefore there cannot be a case that
    // circumvents `applyChange` usage.
    def apply[A](source: IEvent[T, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) =>
            logEvent(s"${indent}pull $source  (new ? false; state = ${res.state})")
            if (res.hasFull) {
              res.full.asInstanceOf[Option[A]]
            } else {
              val v = source.pullUpdate(this)
              res.setFull(v)
              v
            }
          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val v   = source.pullUpdate(this)
            val res = new Value
            res.setFull(v)
            pullMap += ((source, res))
            v
        }
      } finally {
        decIndent()
      }
    }

//    def TEST_PURGE_CACHE(): Unit = pullMap = pullMap.empty

    private[this] var nonCachedTerms = List.empty[IEvent[T, Any]]
    private[this] var nonCachedPath  = List.empty[IEvent[T, Any]]

    def nonCached[A](source: IEvent[T, Any])(body: => A): A = {
      val oldTerms  = nonCachedTerms
      val oldPath   = nonCachedPath
      nonCachedTerms  = source :: oldTerms
      nonCachedPath   = Nil
      try {
        body
      } finally {
        nonCachedTerms  = oldTerms
        nonCachedPath   = oldPath
      }
    }

    private def performPullChange[A](source: IChangeEvent[T, A], res: Value)(implicit phase: Phase): A = {
      if (nonCachedTerms.contains(source)) {
        // Console.err.println(s"erasing non-cached path of size ${nonCachedPath.size} for $source")
        // remove cached values in the current call sequence
        pullMap --= nonCachedPath
        source.pullChange(this)
      } else {
        val oldPath = nonCachedPath
        nonCachedPath = source :: oldPath
        val v = try {
          source.pullChange(this)
        } finally {
          nonCachedPath = oldPath
        }
        if (phase.isNow) res.setNow(v) else res.setBefore(v)
        v
      }
    }

    def expr[A](in: IExpr[T, A])(implicit phase: Phase): A = {
      val inCh = in.changed
      if (contains(inCh)) applyChange(inCh)
      else                in.value
    }

    // caches pulled values
    def applyChange[A](source: IChangeEvent[T, A])(implicit phase: Phase): A = {
      incIndent()
      try {
        val resOpt  = pullMap.get(source)
        val res     = resOpt.getOrElse {
          val _res = new Value
          pullMap += ((source, _res))
          _res
        }
        val isOld   = resOpt.isDefined

        logEvent(s"${indent}pull $source  (old ? $isOld; state = ${res.state})")
        if      (isOld && res.hasPhase      ) res.resolvePhase[A]
        else if (isOld && res.full.isDefined) res.resolveFull [A]
        else {
          performPullChange(source, res)
        }
      } finally {
        decIndent()
      }
    }
  }

  private val currentPull = new ThreadLocal[IMap[Any, IPull[_]]]() {
    override def initialValue(): IMap[Any, IPull[_]] = IMap.empty
  }

  def tryPull[T <: Exec[T], A](source: IEvent[T, A])(implicit tx: T): Option[A] =
    currentPull.get().get(tx).flatMap { p =>
      val pc = p.asInstanceOf[IPull[T]]
      if (pc.contains(source)) pc(source) else None
    }
}

object IPull {
  sealed trait Phase {
    def isBefore: Boolean
    def isNow   : Boolean
  }
  case object Before extends Phase { def isBefore = true  ; def isNow = false }
  case object Now    extends Phase { def isBefore = false ; def isNow = true  }
}
trait IPull[T <: Exec[T]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  def resolveChange[A](implicit phase: IPull.Phase): A

  def resolveExpr[A](in: IExpr[T, A])(implicit phase: IPull.Phase): A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: IEvent[T, Any]): IPush.Parents[T]

  /** Pulls the update from the given source. */
  def apply[A](source: IEvent[T, A]): Option[A]

  def applyChange[A](source: IChangeEvent[T, A])(implicit phase : Phase): A

  /** Pulls the value from the given expression. If `in` is
    * part of the event graph, pulls the update, otherwise returns the current value. */
  def expr[A](in: IExpr[T, A])(implicit phase : Phase): A

  /** Whether the selector has been visited during the push phase. */
  def contains(source: IEvent[T, Any]): Boolean

  def isOrigin(source: IEvent[T, Any]): Boolean

  /** Marks a region of the pull action as non-caching.
    * This is done by submitting a terminal symbol `source`, typically
    * an instance of `It.Expanded`. When anything within the `body` tries to apply
    * the `source`, all values from events on the call tree will be removed
    * from cache.
    */
  def nonCached[A](source: IEvent[T, Any])(body: => A): A

//  def TEST_PURGE_CACHE(): Unit
}