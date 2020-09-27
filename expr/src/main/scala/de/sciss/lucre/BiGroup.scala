/*
 *  BiGroup.scala
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

import de.sciss.lucre.geom.LongSquare
import de.sciss.lucre.impl.{BiGroupImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.span.SpanLike
import de.sciss.{model => m}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroup extends Obj.Type {
  final val MaxSquare     : LongSquare  = LongSquare(0, 0, 0x2000000000000000L)
  final val MaxSide       : Long        = MaxSquare.side
  final val MinCoordinate : Long        = MaxSquare.left
  final val MaxCoordinate : Long        = MaxSquare.right

  final val typeId = 27

  override def init(): Unit = {
    super.init()
    Entry.init()
  }

  // ---- updates ----

  final case class Update[T <: Txn[T], A, +Repr <: BiGroup[T, A]](group: Repr, changes: List[Change[T, A]])

  sealed trait Change[T <: Txn[T], +A] {
    def elem: Entry[T, A]
  }

  final case class Added[T <: Txn[T], A](span: SpanLike /* Span.HasStart */ , elem: Entry[T, A])
    extends Change[T, A]

  final case class Removed[T <: Txn[T], A](span: SpanLike /* Span.HasStart */ , elem: Entry[T, A])
    extends Change[T, A]

  final case class Moved[T <: Txn[T], A](change: m.Change[SpanLike], elem: Entry[T, A])
    extends Change[T, A]

  // ---- structural data ----

  type Leaf[T <: Txn[T], +A] = (SpanLike /* Span.HasStart */ , Vec[Entry[T, A]])

  // Note: we use `Obj` instead of `Elem` because views may need to
  // store an `.id`!
  object Entry extends Obj.Type {
    final val typeId = 28

    def unapply[T <: Txn[T], A](entry: Entry[T, A]): Entry[T, A] = entry

    implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, Entry[T, A]] =
      Impl.entryFormat[T, A]

    def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
      Impl.readIdentifiedEntry(in)
  }

  trait Entry[T <: Txn[T], +A] extends Obj[T] with Publisher[T, m.Change[SpanLike]] {
    def span : SpanLikeObj[T]
    def value: A

    // improved support for pattern matching
    def isEmpty: Boolean = false
    def get: (SpanLikeObj[T], A) = (span, value)
  }

  object Modifiable {
    implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiGroup.Modifiable[T, A]] =
      Impl.modifiableFormat[T, A]

    def apply[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
      Impl.newModifiable[T, E]

    def read[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): Modifiable[T, A] =
      format[T, A].readT(in)
  }

  trait Modifiable[T <: Txn[T], A] extends BiGroup[T, A] {
    def add(span: SpanLikeObj[T], elem: A)(implicit tx: T): Entry[T, A]

    def remove(span: SpanLikeObj[T], elem: A)(implicit tx: T): Boolean

    def clear()(implicit tx: T): Unit

    override def changed: EventLike[T, BiGroup.Update[T, A, Modifiable[T, A]]]
  }

  implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiGroup[T, A]] =
    Impl.format[T, A]

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)
}

trait BiGroup[T <: Txn[T], A] extends Obj[T] with Publisher[T, BiGroup.Update[T, A, BiGroup[T, A]]] {

  import BiGroup.Leaf

  def modifiableOption: Option[BiGroup.Modifiable[T, A]]

  def iterator(implicit tx: T): Iterator[Leaf[T, A]]

  /** Returns `true` if not a single element is contained in the collection. */
  def isEmpty(implicit tx: T): Boolean

  /** Returns `true` if at least one element is contained in the collection. */
  def nonEmpty(implicit tx: T): Boolean

  /** Queries all elements intersecting a given point in time.
   * That is, returns an iterator of all elements whose span contains the time point
   * `(span start <= time && span.stop > time)`
   *
   * This methods makes no guarantees about the ordering of the returned iterator.
   *
   * @param time the point in time to search at
   * @return  a (possibly empty) iterator of the intersecting elements
   */
  def intersect(time: Long)(implicit tx: T): Iterator[Leaf[T, A]]

  /** Queries all elements intersecting a given time span.
   * That is, returns an iterator of all elements whose span contains or partly overlaps the query span.
   * `(span start < query.stop && span.stop > query.start)`
   *
   * This methods makes no guarantees about the ordering of the returned iterator.
   *
   * @param span the the span to search within (this may be a half-bounded interval or even `Span.All`)
   * @return  a (possibly empty) iterator of the intersecting elements
   */
  def intersect(span: SpanLike)(implicit tx: T): Iterator[Leaf[T, A]]

  /** Performs a range query according to separate intervals for the allowed start and stop positions
   * of the element spans. That is, returns an iterator of all elements whose span satisfies the
   * constraints given for start and stop positions
   * `(start.contains( elem.span.start ) && stop.contains( elem.span.stop ))`
   *
   * Both for the start and stop constraint, half-bounded or unbounded (`Span.All`) intervals can be used.
   * Examples
   *
   * - to find all elements which start between 10 (inclusive) and 20 (exclusive), use `start = Span( 10, 20 ), stop = Span.All`.
   * - to find all elements which start before (<) 10 and stop from (>=) 20, use `start = Span.until( 10 ), stop = Span.from( 20 )`.
   *
   * This methods makes no guarantees about the ordering of the returned iterator.
   *
   * @param start   the constraint for the start position of the spans of the elements filtered.
   * @param stop    the constraint for the stop position of the spans of the elements filtered.
   * @return  a (possibly empty) iterator of the intersecting elements
   */
  def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: T): Iterator[Leaf[T, A]]

  /** Queries the closest event (an element's span starting or stopping) later than the given time
   *
   * @param time the query time
   * @return a time, greater than the query time, at which the next event occurs, or `None` if
   *         there are no events after the query time
   */
  def eventAfter(time: Long)(implicit tx: T): Option[Long]

  /** Queries the closest event (an element's span starting or stopping) earlier than the given time
   *
   * @param time the query time
   * @return a time, smaller than the query time, at which the previous event occurs, or `None` if
   *         there are no events before the query time
   */
  def eventBefore(time: Long)(implicit tx: T): Option[Long]

  /** Finds the first occurring event, if there is any. Ignores objects with `Span.All`. */
  def firstEvent(implicit tx: T): Option[Long]

  /** Finds the last occurring event, if there is any. Ignores objects with `Span.All`. */
  def lastEvent(implicit tx: T) : Option[Long]

  /** Queries all elements which produce an event (starting or stopping) at a given time.
   *
   * @param time the time instant for which to gather the events
   * @return  a tuple of two iterators. the first iterator (`_1`) contains the events which
   *          start at the query time, the second iterator (`_2`) contains the event which
   *          stop at the query time
   */
  def eventsAt(time: Long)(implicit tx: T): (Iterator[Leaf[T, A]], Iterator[Leaf[T, A]])

  /** Finds the leaf for a given span value (if it exists). */
  def get(span: SpanLike)(implicit tx: T): Vec[BiGroup.Entry[T, A]]

  /** Tries to recover the actual object of an element's position, given only
   * an evaluated span. The result may for example be used in a subsequent removal of the element.
   */
  def recoverSpan(span: SpanLike, elem: A)(implicit tx: T): Option[SpanLikeObj[T]]

  def debugList(implicit tx: T): List[(SpanLike, A)]

  def debugPrint(implicit tx: T): String
}