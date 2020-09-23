/*
 *  ContextMixin.scala
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
package expr
package impl

import de.sciss.lucre.TxnLike.peer
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.graph.{Control, It}

import scala.annotation.tailrec
import scala.concurrent.stm.{Ref, TMap, TxnLocal}

trait ContextMixin[T <: Txn[T]] extends Context[T] {
  // ---- abstract ----

  protected def selfH: Option[Source[T, Obj[T]]]

  // ---- impl ----

  final val targets: ITargets[T] = ITargets[T]

  private type SourceMap  = Map [AnyRef, Disposable[T]]
  private type Properties = TMap[AnyRef, Map[String, Any]]

  private[this] val globalMap : Ref[SourceMap]  = Ref(Map.empty)
  private[this] val properties: Properties      = TMap.empty

  def initGraph(g: Graph)(implicit tx: T): Unit = {
    properties.clear()
    g.controls.foreach { conf =>
      properties.put(conf.control.token, conf.properties)
    }
  }

  private[this] val terminals = TxnLocal(List.empty[Nested])
  private[this] val markers   = TxnLocal(Set .empty[Int   ])

  private final class Nested(val ref: AnyRef, val level: Int) {
    var sourceMap: SourceMap = Map.empty
  }

  def nested[A](it: It.Expanded[T, _])(body: => A)(implicit tx: T): (A, Disposable[T]) = {
    val tOld    = terminals()
    val n       = new Nested(it.ref, level = tOld.size)
    terminals() = tOld :+ n // n :: tOld
    val res     = body
    terminals() = tOld
    markers.transform(_ - n.level)

    val disposableIt = n.sourceMap.values
    val disposable: Disposable[T] =
      if (disposableIt.isEmpty) Disposable.empty
      else disposableIt.toList match {
        case single :: Nil  => single
        case more           => Disposable.seq(more: _*)
      }

    (res, disposable)
  }

  def dispose()(implicit tx: T): Unit = {
    require (terminals().isEmpty, "Must not call dispose in a nested operation")
    val m = globalMap.swap(Map.empty)
    m.foreach(_._2.dispose())
  }

  final def visit[U <: Disposable[T]](ref: AnyRef, init: => U)(implicit tx: T): U = {
    val t = terminals()
    if (t.nonEmpty) t.find(_.ref == ref) match {
      case Some(n)  => markers.transform(_ + n.level)
      case None     =>
    }

    globalMap().get(ref) match {
      case Some(res) => res.asInstanceOf[U]  // not so pretty...
      case None =>
        @tailrec
        def loop(rem: List[Nested]): U =
          rem match {
            case head :: tail =>
              head.sourceMap.get(ref) match {
                case Some(res)  => res.asInstanceOf[U]
                case None       => loop(tail)
              }

            case Nil =>
              val oldMarkers  = markers.swap(Set.empty)
              val exp         = init
              val newMarkers  = markers()
              if (newMarkers.isEmpty) {
                globalMap.transform(_ + (ref -> exp))
                markers()     = oldMarkers
              } else {
                val m         = newMarkers.max
                val n         = terminals().apply(m)
                n.sourceMap  += (ref -> exp)
                markers()     = oldMarkers union newMarkers
              }
              exp
          }

        loop(t)
    }
  }

  def selfOption(implicit tx: T): Option[Obj[T]] = selfH.map(_.apply())

  def getProperty[A](c: Control, key: String)(implicit tx: T): Option[A] = {
    val m0: Map[String, Any] = properties.get(c.token).orNull
    if (m0 == null) None else {
      m0.get(key).asInstanceOf[Option[A]]
    }
  }
}

final class ContextImpl[T <: Txn[T]](protected val selfH: Option[Source[T, Obj[T]]],
                                     val attr: Context.Attr[T])
                                    (implicit val workspace: Workspace[T], val cursor: Cursor[T],
                                     val undoManager: UndoManager[T])
  extends ContextMixin[T]