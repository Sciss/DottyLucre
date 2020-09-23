/*
 *  ListObj.scala
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

import de.sciss.lucre.impl.{ListObjImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

object ListObj extends Obj.Type {
  final val typeId  = 23

  final case class Update[T <: Txn[T], A, +Repr <: ListObj[T, A]](list: Repr, changes: Vec[Change[A]])

  sealed trait Change[+A] {
    def index: Int
    def elem: A
  }

  final case class Added[A](index: Int, elem: A)
    extends Change[A]

  final case class Removed[A](index: Int, elem: A)
    extends Change[A]

  object Modifiable {
    /** Returns a format for a modifiable list. */
    implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, Modifiable[T, A]] =
      Impl.modFormat[T, A]

    def read[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): Modifiable[T, A] =
      format[T, A].readT(in)

    /** Creates a new empty linked list. */
    def apply[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
      Impl.newModifiable[T, E]
  }

  /** Modifiable extension of the linked list. Elements can be appended or prepended in O(1).
   * Removal of the head or last element is O(1). Arbitrary removal takes O(N).
   */
  trait Modifiable[T <: Txn[T], A] extends ListObj[T, A] with Event.Node[T] {
    def addLast(elem: A): Unit
    def addHead(elem: A): Unit

    def removeLast(): A
    def removeHead(): A

    def insert  (index: Int, elem: A): Unit
    def remove  (elem: A): Boolean
    def removeAt(index: Int): A

    def clear(): Unit

    override def changed: EventLike[T, Update[T, A, Modifiable[T, A]]]
  }

  implicit def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, ListObj[T, A]] =
    Impl.format[T, A]

  def read[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): ListObj[T, A] =
    format[T, A].readT(in)

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)
}

/** An observable linked list with fast `head` and `last` operations.
 * This is the read-only layer, see `List.Modifiable` for a mutable list.
 *
 * The list will report insertions and deletions.
 *
 * @tparam A      the element type of the list
 */
trait ListObj[T <: Txn[T], A] extends Obj[T] with Publisher[T, ListObj.Update[T, A, ListObj[T, A]]] {
  def isEmpty : Boolean
  def nonEmpty: Boolean
  def size    : Int

  def apply(index: Int): A
  def get  (index: Int): Option[A]

  def headOption: Option[A]
  def lastOption: Option[A]

  def head: A
  def last: A

  def iterator: Iterator[A]

  def modifiableOption: Option[ListObj.Modifiable[T, A]]

  /** Note: this is an O(n) operation. */
  def indexOf(elem: A): Int
}