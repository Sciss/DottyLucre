/*
 *  BiPin.scala
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

import de.sciss.lucre.impl.{BiPinImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.{model => m}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiPin extends Obj.Type {
  final val typeId = 25

  override def init(): Unit = {
    super.init()
    Entry.init()
  }

  final case class Update[T <: Txn[T], A, +Repr <: BiPin[T, A]](pin: Repr, changes: List[Change[T, A]])

  object Entry extends Elem.Type {
    final val typeId = 26

    def unapply[T <: Txn[T], A](entry: Entry[T, A]): Option[(LongObj[T], A)] =
      Some((entry.key, entry.value))

    def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Elem[T] =
      Impl.readIdentifiedEntry(in)

    implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, Entry[T, A]] =
      Impl.entryFormat[T, A]
  }
  trait Entry[T <: Txn[T], +A] extends Elem[T] with Publisher[T, m.Change[Long]] {
    def key  : LongObj[T]
    def value: A
  }

  type Leaf[T <: Txn[T], +A] = Vec[Entry[T, A]]

  sealed trait Change[T <: Txn[T], A] {
    def entry: Entry[T, A]
  }

  final case class Added  [T <: Txn[T], A](time: Long          , entry: Entry[T, A]) extends Change[T, A]
  final case class Removed[T <: Txn[T], A](time: Long          , entry: Entry[T, A]) extends Change[T, A]
  final case class Moved  [T <: Txn[T], A](time: m.Change[Long], entry: Entry[T, A]) extends Change[T, A]

  object Modifiable {
    /** Extractor to check if a `BiPin` is actually a `BiPin.Modifiable`. */
    def unapply[T <: Txn[T], A](v: BiPin[T, A]): Option[Modifiable[T, A]] = {
      if (v.isInstanceOf[Modifiable[_, _]]) Some(v.asInstanceOf[Modifiable[T, A]]) else None
    }

    def read[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): Modifiable[T, A] =
      format[T, A].readT(in)

    def apply[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
      Impl.newModifiable[T, E]

    implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiPin.Modifiable[T, A]] =
      Impl.modifiableFormat[T, A]
  }

  trait Modifiable[T <: Txn[T], A] extends BiPin[T, A] {
    def add   (key: LongObj[T], value: A)(implicit tx: T): Unit
    def remove(key: LongObj[T], value: A)(implicit tx: T): Boolean
    def clear()(implicit tx: T): Unit

    override def changed: EventLike[T, Update[T, A, Modifiable[T, A]]]
  }

  def read[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): BiPin[T, A] =
    format[T, A].readT(in)

  implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiPin[T, A]] =
    Impl.format[T, A]

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)
}

trait BiPin[T <: Txn[T], A] extends Obj[T] with Publisher[T, BiPin.Update[T, A, BiPin[T, A]]] {
  import BiPin.{Entry, Leaf}

  def modifiableOption: Option[BiPin.Modifiable[T, A]]

  /** Returns `true` if not a single element is contained in the collection. */
  def isEmpty(implicit tx: T): Boolean

  /** Returns `true` if at least one element is contained in the collection. */
  def nonEmpty(implicit tx: T): Boolean

  /** Queries the element valid for the given point in time.
   * Unlike, `intersect`, if there are multiple elements sharing
   * the same point in time, this returns the most recently added element.
   *
   * We propose that this should be the unambiguous way to evaluate
   * the `BiPin` for a given moment in time.
   *
   * @param time the query time point
   * @return  an element for the given time point, if it exists, otherwise `None`
   */
  def at(time: Long)(implicit tx: T): Option[Entry[T, A]]

  def valueAt(time: Long)(implicit tx: T): Option[A]

  /** Finds the entry at the given time, or the closest entry before the given time.
   *
   * @param time the query time
   * @return     the entry nearest in time to the query time, but not later than the
   *             query time, or `None` if there is no entry at such time
   */
  def floor(time: Long)(implicit tx: T): Option[Entry[T, A]]

  /** Finds the entry at the given time, or the closest entry after the given time.
   *
   * @param time the query time
   * @return     the entry nearest in time to the query time, but not earlier than the
   *             query time, or `None` if there is no entry at such time
   */
  def ceil(time: Long)(implicit tx: T): Option[Entry[T, A]]

  /** Queries all elements which are found at a given point in time.
   * There may be multiple time expressions which are not equal but
   * evaluate to the same moment in time. It is thus possible that
   * for a given point, multiple elements are found.
   *
   * @param time the query point
   * @return  the sequence of elements found along with their time expressions
   */
  def intersect(time: Long)(implicit tx: T): Leaf[T, A]

  /** Finds the entry with the smallest time which is greater than the query time.
   *
   * @param time the query time
   * @return     the time corresponding to the next entry, or `None` if there is no entry
   *             later than the given time
   */
  def eventAfter(time: Long)(implicit tx: T): Option[Long]

  /** Finds the entry with the greatest time which is less than the query time.
   *
   * @param time the query time
   * @return     the time corresponding to the next entry, or `None` if there is no entry
   *             earlier than the given time
   */
  def eventBefore(time: Long)(implicit tx: T): Option[Long]

  def debugList(implicit tx: T): List[(Long, A)]
}