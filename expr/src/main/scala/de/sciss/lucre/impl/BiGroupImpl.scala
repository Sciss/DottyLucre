/*
 *  BiGroupImpl.scala
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
package impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.data.{DetSkipOctree, SkipOctree}
import de.sciss.lucre.geom.LongSpace.TwoDim
import de.sciss.lucre.geom.{DistanceMeasure, LongDistanceMeasure2D, LongPoint2D, LongPoint2DLike, LongRectangle, LongSquare}
import de.sciss.model.Change
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}
import de.sciss.span.{Span, SpanLike}

import scala.annotation.elidable
import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroupImpl {
  import BiGroup.{Entry, Leaf, MaxCoordinate, MaxSide, MaxSquare, MinCoordinate, Modifiable}

  def spanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop     )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop     )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate) // ?? what to do with this case ?? forbid?
  }

  def searchSpanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop         )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate + 1)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop         )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate + 1)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate    ) // ?? what to do with this case ?? forbid?
  }

  final def intersectTime[T <: Txn[T], A](tree: Tree[T, A])(time: Long)(implicit tx: T): Iterator[A] = {
    val start = time
    val stop = time + 1
    //         val shape = Rectangle( ti, MinCoordinate, MaxCoordinate - ti + 1, ti - MinCoordinate + 1 )
    // horizontally: until query_stop; vertically: from query_start
    // start < query.stop && stop > query.start
    val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
    tree.rangeQuery(shape)
  }

  final def intersectSpan[T <: Txn[T], A](tree: Tree[T, A])(span: SpanLike)(implicit tx: T): Iterator[A] = {
    // horizontally: until query_stop; vertically: from query_start
    span match {
      case Span(start, stop) =>
        val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.From(start) =>
        val shape = LongRectangle(MinCoordinate, start + 1, MaxSide, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.Until(stop) =>
        val shape = LongRectangle(MinCoordinate, MinCoordinate, stop - MinCoordinate, MaxSide)
        tree.rangeQuery(shape)

      case Span.All  => tree.iterator
      case Span.Void => Iterator.empty
    }
  }

  final def rangeSearch[T <: Txn[T], A](tree: Tree[T, A])(start: SpanLike, stop: SpanLike)
                                       (implicit tx: T): Iterator[A] = {
    if (start === Span.Void || stop === Span.Void) return Iterator.empty

    val startP = searchSpanToPoint(start)
    val stopP  = searchSpanToPoint(stop)
    val shape  = LongRectangle(startP.x, stopP.x, startP.y - startP.x /* + 1 */ , stopP.y - stopP.x /* + 1 */)
    //println( "RANGE " + shape )
    tree.rangeQuery(shape)
  }

  // this can be easily implemented with two rectangular range searches
  final def eventsAt[T <: Txn[T], A](tree: Tree[T, A])(time: Long)(implicit tx: T): (Iterator[A], Iterator[A]) = {
    val startShape = LongRectangle(time, MinCoordinate, 1, MaxSide)
    val stopShape  = LongRectangle(MinCoordinate, time, MaxSide, 1)
    (tree.rangeQuery(startShape), tree.rangeQuery(stopShape))
  }

  final def eventAfter[T <: Txn[T], T2](tree: Tree[T, (SpanLike, T2)])(time: Long)(implicit tx: T): Option[Long] = {
    val t1    = time + 1
    val point = LongPoint2D(t1, t1) // + 1
    val span  = tree.nearestNeighborOption(point, AdvanceNextNeighborMetric).map(_._1).getOrElse(Span.Void)
    span match {
      case sp @ Span.From(start) => assert(start >= t1, sp); Some(start) // else None
      case sp @ Span.Until(stop) => assert(stop  >= t1, sp); Some(stop ) // else None
      case sp @ Span(start, stop) =>
        if (start >= t1) {
          Some(start)
        } else {
          assert(stop >= t1, sp)
          Some(stop)
        }
      case _ => None // All or Void
    }
  }

  final def eventBefore[T <: Txn[T], T2](tree: Tree[T, (SpanLike, T2)])(time: Long)(implicit tx: T): Option[Long] = {
    val t1    = time - 1
    val point = LongPoint2D(t1, t1)
    val span  = tree.nearestNeighborOption(point, RegressNextNeighborMetric).map(_._1).getOrElse(Span.Void)
    span match {
      case sp @ Span.From(start)  => assert(start <= t1, sp); Some(start) // else None
      case sp @ Span.Until(stop)  => assert(stop  <= t1, sp); Some(stop ) // else None
      case sp @ Span(start, stop) =>
        if (stop <= t1) {
          Some(stop)
        } else {
          assert(start <= t1, sp)
          Some(start)
        }
      case _ => None // All or Void
    }
  }

  // ... accepted are points with x > LRP || y > LRP ...
  final val AdvanceNextNeighborMetric: DistanceMeasure.Ops[Long, LongPoint2DLike, LongSquare] = 
    LongDistanceMeasure2D.nextSpanEvent(MaxSquare)
  
  final val RegressNextNeighborMetric: DistanceMeasure.Ops[Long, LongPoint2DLike, LongSquare] =
    LongDistanceMeasure2D.prevSpanEvent(MaxSquare)

  var showLog = false

  @elidable(elidable.CONFIG) private def log(what: => String): Unit =
    if (showLog) println(s"<bigroup> $what")

  type Tree    [T <: Txn[T], A] = SkipOctree[T, LongPoint2DLike, LongPoint2D, LongSquare, A]

  implicit def spanLikeFormat: ConstFormat[SpanLike] = ??? // XXX TODO

  type LeafImpl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]] = (SpanLike, Vec[Entry[T, E[T]]])

  type TreeImpl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]] = SkipOctree[T, LongPoint2DLike, LongPoint2D, LongSquare, LeafImpl[T, E]]

  def verifyConsistency[T <: Txn[T], A](group: BiGroup[T, A], reportOnly: Boolean)(implicit tx: T): Vec[String] =
    group match {
      case impl: Impl[T, _, _] =>   // wrong warning
        impl.treeHandle match {
          case t: DetSkipOctree[T, _, _, _, _] =>
            t.verifyConsistency(reportOnly)
          case _ => sys.error("Not a deterministic octree implementation")
        }
    }

  def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiGroup[T, A]] = anyFmt.asInstanceOf[Fmt[T, A]]

  private val anyFmt = new Fmt[AnyTxn, Obj[AnyTxn]]

  def modifiableFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiGroup.Modifiable[T, A]] =
    anyModFmt.asInstanceOf[ModFmt[T, A]]

  private val anyModFmt = new ModFmt[AnyTxn, Obj[AnyTxn]]

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val targets = Targets.read(in)
    read(in, targets)
  }

  def readIdentifiedEntry[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val targets = Targets.read(in)
    readEntry(in, targets)
  }

  private class Fmt[T <: Txn[T], A <: Elem[T]] extends ObjFormat[T, BiGroup[T, A]] {
    def tpe: Obj.Type = BiGroup
  }

  private class ModFmt[T <: Txn[T], A <: Elem[T]] extends ObjFormat[T, BiGroup.Modifiable[T, A]] {
    def tpe: Obj.Type = BiGroup
  }

  private[lucre] final class EntryImpl[T <: Txn[T], A <: Elem[T]](val targets : Targets[T],
                                                                  val span    : SpanLikeObj[T],
                                                                  val value   : A
                                                                 )
    extends SingleEventNode[T, Change[SpanLike]] with Entry[T, A] {

    def tpe: Obj.Type = Entry

    override def toString = s"Entry($id, $span, $value)"

    object changed extends Changed with RootEvent[T, Change[SpanLike]]

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new EntryImpl(Targets[Out](), context(span), context[Elem](value)).connect()

    protected def writeData(out: DataOutput): Unit = {
      span .write(out)
      value.write(out)
    }

    protected def disposeData()(implicit tx: T): Unit = disconnect()

    def connect()(implicit tx: T): this.type = {
      log(s"$this.connect()")
      span.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: T): Unit = {
      log(s"$this.disconnect()")
      span.changed -/-> changed
    }
  }

  implicit def entryFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, Entry[T, A]] =
    anyEntryFmt.asInstanceOf[EntryFmt[T, A]]

  private val anyEntryFmt = new EntryFmt[AnyTxn, Obj[AnyTxn]]

  private def readEntry[T <: Txn[T], A <: Elem[T]](in: DataInput, targets: Targets[T])
                                                  (implicit tx: T): EntryImpl[T, A] = {
    val span  = SpanLikeObj.read(in)
    val value = Elem       .read(in).asInstanceOf[A]
    new EntryImpl(targets, span, value)
  }

  private final class EntryFmt[T <: Txn[T], A <: Elem[T]] extends ObjFormat[T, Entry[T, A]] {
    def tpe: Obj.Type = Entry
  }

  final def copyTree[In <: Txn[In], Out <: Txn[Out], E[~ <: Txn[~]] <: Elem[~],
    Repr <: Impl[Out, E, Repr]](in: TreeImpl[In, E], out: TreeImpl[Out, E], outImpl: Repr)
                               (implicit txIn: In, txOut: Out, context: Copy[In, Out]): Unit = {
    type EntryAux[~ <: Txn[~]] = Entry[~, E[~]]
    in.iterator.foreach { case (span, xsIn) =>
      val xsOut: Vec[EntryAux[Out]] = xsIn.map(entry => context[EntryAux](entry))
      out.add(span -> xsOut)
      xsOut.foreach { entry =>
        outImpl.changed += entry
      }
    }
  }

  abstract class Impl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~], Repr <: BiGroup.Modifiable[T, E[T]]]
    extends Modifiable[T, E[T]]
      with SingleEventNode[T, BiGroup.Update[T, E[T], Repr]] {

    group: Repr =>

    type A = E[T]

    // ---- abstract ----

    implicit final def pointView: (Leaf[T, A], T) => LongPoint2DLike = (tup, _) => spanToPoint(tup._1)

    protected def tree: TreeImpl[T, E]

    // ---- implemented ----

    // DO NOT DEFINE THE TYPE, BECAUSE THIS IS
    // USED AS A MIXIN, E.G. BY TIMELINE
    // final def tpe: Obj.Type = BiGroup

    final def isEmpty (implicit tx: T): Boolean = tree.isEmpty
    final def nonEmpty(implicit tx: T): Boolean = !isEmpty

    final def treeHandle: TreeImpl[T, E] = tree

    // Note: must be after `EntryFmt`
    protected final def newTree()(implicit tx: T): TreeImpl[T, E] =
      SkipOctree.empty[T, LongPoint2DLike, LongPoint2D, LongSquare, LeafImpl[T, E]](BiGroup.MaxSquare)

    // Note: must be after `EntryFmt`
    protected final def readTree(in: DataInput)(implicit tx: T): TreeImpl[T, E] =
      SkipOctree.read[T, LongPoint2DLike, LongPoint2D, LongSquare, LeafImpl[T, E]](in)

    // ---- event behaviour ----

    object changed extends Changed with GeneratorEvent[T, BiGroup.Update[T, A, Repr]] with Caching {

      def += (entry: Entry[T, A])(implicit tx: T): Unit = entry.changed ---> this
      def -= (entry: Entry[T, A])(implicit tx: T): Unit = entry.changed -/-> this

      override def pullUpdate(pull: Pull[T])(implicit tx: T): Option[BiGroup.Update[T, A, Repr]] = {
        if (pull.isOrigin(this)) return Some(pull.resolve)

        val par = pull.parents(this)
        log(s"$this.pullUpdate -> parents = $par")

        val changes: List[BiGroup.Moved[T, A]] = par.iterator.flatMap { evt =>
          val entry = evt.node.asInstanceOf[Entry[T, A]]
          val ch0   = pull(entry.changed)
          log(s"$this.pullUpdate -> from entry $entry pulled $ch0")
          ch0.map {
            case ch @ Change(spanValOld, spanValNew) =>
              assert(removeNoFire(spanValOld, entry))
              addNoFire          (spanValNew, entry)
              BiGroup.Moved(ch, entry)
          }
        } .toList

        if (changes.isEmpty) None else Some(BiGroup.Update(group, changes))
      }
    }

    final protected def disposeData()(implicit tx: T): Unit = {
      tree.iterator.foreach { case (_, xs) =>
        xs.foreach(_.dispose())
      }
      tree.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = tree.write(out)

    // ---- collection behaviour ----

    final def clear()(implicit tx: T): Unit = {
      val changes = tree.iterator.toList.flatMap {
        case (spanVal, seq) =>
          seq.map {
            entry => BiGroup.Removed(spanVal, entry)
          }
      }
      tree.clear()
      if (changes.nonEmpty) changed.fire(BiGroup.Update(group, changes))(tx)
    }

    final def add(span: SpanLikeObj[T], elem: A)(implicit tx: T): Entry[T, A] = {
      log(s"$this.add($span, $elem)")
      val spanVal = span.value
      val tgt     = Targets[T]()(tx)
      val entry   = new EntryImpl[T, A](tgt, span, elem).connect()
      addNoFire(spanVal, entry)

      changed += entry
      changed.fire(BiGroup.Update(group, BiGroup.Added(spanVal, entry) :: Nil))(tx)

      entry
    }

    private def addNoFire(spanVal: SpanLike, entry: Entry[T, A])(implicit tx: T): Unit = {
      val point = spanToPoint(spanVal)
      //if( showLog ) println( "add at point " + point )
      //         val entry   = (span, elem)
      tree.transformAt(point) {
        case None           => Some(spanVal -> Vector (entry))
        case Some((_, seq)) => Some(spanVal -> (seq :+ entry))
      }
    }

    final def recoverSpan(spanVal: SpanLike, elem: A)(implicit tx: T): Option[SpanLikeObj[T]] = {
      val point = spanToPoint(spanVal)
      tree.get(point).flatMap { case (_, vec) =>
        vec.collectFirst {
          case e if e.value === elem => e.span
        }
      }
    }

    final def get(spanVal: SpanLike)(implicit tx: T): Vec[BiGroup.Entry[T, A]] = {
      val point = spanToPoint(spanVal)
      tree.get(point).fold[Vec[BiGroup.Entry[T, A]]](Vector.empty)(_._2)
    }

    final def remove(span: SpanLikeObj[T], elem: A)(implicit tx: T): Boolean = {
      val spanVal   = span.value
      val point     = spanToPoint(spanVal)
      val entryOpt  = tree.get(point).flatMap {
        case (_, Vec(single)) =>
          if (single.span === span && single.value === elem) {
            tree.removeAt(point)
            Some(single)
          } else {
            None
          }
        case (_, seq) =>
          val (equal, diff) = seq.partition(entry => entry.span === span && entry.value === elem)
          if (equal.nonEmpty) {
            tree.add((spanVal, diff))
            equal.headOption
          } else {
            None
          }
      }

      entryOpt.foreach { entry =>
        changed -= entry
        changed.fire(BiGroup.Update(group, BiGroup.Removed(spanVal, entry) :: Nil))(tx)
        entry.dispose()
      }

      entryOpt.isDefined
    }

    private def removeNoFire(spanVal: SpanLike, entry: Entry[T, A])(implicit tx: T): Boolean = {
      val point = spanToPoint(spanVal)
      val found = tree.get(point)
      found match {
        case Some((_, Vec(single))) =>
          if (single === entry) {
            assert(tree.removeAt(point).isDefined)
            true
          } else {
            false
          }
        case Some((_, seq)) =>
          val seqNew = seq.filterNot(_ === entry)
          if (seqNew.size !== seq.size) {
            assert(tree.add((spanVal, seqNew)))
            true
          } else {
            false
          }
        case None => false
      }
    }

    final def debugList(implicit tx: T): List[(SpanLike, A)] =
      tree.toList.flatMap {
        case (span, seq) => seq.map(span -> _.value)
      }

    final def debugPrint(implicit tx: T): String = tree.debugPrint()

    final def iterator(implicit tx: T): Iterator[Leaf[T, A]] = tree.iterator

    final def intersect(time: Long)(implicit tx: T): Iterator[Leaf[T, A]] =
      BiGroupImpl.intersectTime(tree)(time)

    final def intersect(span: SpanLike)(implicit tx: T): Iterator[Leaf[T, A]] =
      BiGroupImpl.intersectSpan(tree)(span)

    final def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: T): Iterator[Leaf[T, A]] =
      BiGroupImpl.rangeSearch(tree)(start, stop)

    // this can be easily implemented with two rectangular range searches
    final def eventsAt(time: Long)(implicit tx: T): (Iterator[Leaf[T, A]], Iterator[Leaf[T, A]]) =
      BiGroupImpl.eventsAt(tree)(time)

    final def eventAfter(time: Long)(implicit tx: T): Option[Long] =
      BiGroupImpl.eventAfter(tree)(time)

    final def eventBefore(time: Long)(implicit tx: T): Option[Long] =
      BiGroupImpl.eventBefore(tree)(time)

    final def firstEvent(implicit tx: T): Option[Long] =
      BiGroupImpl.eventAfter(tree)(BiGroup.MinCoordinate)

    final def lastEvent(implicit tx: T): Option[Long] =
      BiGroupImpl.eventBefore(tree)(BiGroup.MaxCoordinate)

    protected type GroupAux[~ <: Txn[~]] = BiGroup[~, E[~]]
  }

  def newModifiable[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
    new Impl1[T, E](Targets[T]()) {
      val tree: TreeImpl[T, E] = newTree()(tx)
    }

  private def read[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](in: DataInput, _targets: Targets[T])
                                                          (implicit tx: T): Impl[T, E, Impl1[T, E]] =
    new Impl1[T, E](_targets) {
      val tree: TreeImpl[T, E] = readTree(in)(tx)
    }

  private abstract class Impl1[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](protected val targets: Targets[T])
    extends Impl[T, E, Impl1[T, E]] { in =>

    final def tpe: Obj.Type = BiGroup

    override def toString: String = s"BiGroup${tree.id}"

    def modifiableOption: Option[BiGroup.Modifiable[T, A]] = Some(this)

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      new Impl1[Out, E](Targets[Out]()) { out =>
        val tree: TreeImpl[Out, E] = out.newTree()
        context.defer[GroupAux](in, out)(copyTree[T, Out, E, Impl1[Out, E]](in.tree, out.tree, out))
        // connect()
      }
    }
  }
}