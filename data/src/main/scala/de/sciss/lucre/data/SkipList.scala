/*
 *  SkipList.scala
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

package de.sciss.lucre.data

import de.sciss.lucre.stm.{Base, Ident, Mutable, NewImmutSerializer, TxSerializer, Exec}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object SkipList {
  /** A trait for observing the promotion and demotion of a key
   * in the skip list's level hierarchy
   */
  trait KeyObserver[-Tx, /* @spec(KeySpec) */ -A] {
    /** Notifies the observer that a given key
     * is promoted to a higher (more sparse) level
     */
    def keyUp(key: A)(implicit tx: Tx): Unit

    /** Notifies the observer that a given key
     * is demoted to a lower (more dense) level
     */
    def keyDown(key: A)(implicit tx: Tx): Unit
  }

  // Note: We could also have `object NoKeyObserver extends KeyObserver[ Any, Any ]` if
  // `A` was made contravariant, too. But I guess we would end up in boxing since
  // that wouldn't be specialized any more?
  object NoKeyObserver extends KeyObserver[Any, Any] {
    def keyUp  (key: Any)(implicit tx: Any): Unit = ()
    def keyDown(key: Any)(implicit tx: Any): Unit = ()
  }

  object Set {
    def empty[T <: Exec[T], A](implicit tx: T, ord: scala.Ordering[A],
                               keySerializer: NewImmutSerializer[A]): SkipList.Set[T, A] =
      HASkipList.Set.empty[T, A]

    def empty[T <: Exec[T], A](keyObserver: SkipList.KeyObserver[T, A] = NoKeyObserver)
                              (implicit tx: T, ord: scala.Ordering[/*T,*/ A],
                               keySerializer: NewImmutSerializer[A]): SkipList.Set[T, A] =
      HASkipList.Set.empty[T, A](keyObserver = keyObserver)

    def read[T <: Exec[T], A](in: DataInput, tx: T,
                              keyObserver: SkipList.KeyObserver[T, A] = NoKeyObserver)
                             (implicit acc: tx.Acc, ord: scala.Ordering[/*T,*/ A],
                              keySerializer: NewImmutSerializer[A]): SkipList.Set[T, A] =
      HASkipList.Set.read[T, A](in, tx, keyObserver)

    implicit def serializer[T <: Exec[T], A](keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                            (implicit ord: scala.Ordering[/*T,*/ A],
                                             keySerializer: NewImmutSerializer[A]): TxSerializer[T, Set[T, A]] =
      new SetSer[T, A](keyObserver)

    private final class SetSer[T <: Exec[T], A](keyObserver: SkipList.KeyObserver[T, A])
                                               (implicit ord: scala.Ordering[/*T,*/ A],
                                                keySerializer: NewImmutSerializer[A])
      extends TxSerializer[T, Set[T, A]] {

      override def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Set[T, A] = 
        Set.read[T, A](in, tx, keyObserver)

      override def write(list: Set[T, A], out: DataOutput): Unit = 
        list.write(out)

      override def toString = "SkipList.Set.serializer"
    }
  }

  object Map {
    def empty[T <: Exec[T], A, B](implicit tx: T, ord: scala.Ordering[/*T,*/ A],
                                  keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A],
                                  valueSerializer: TxSerializer[T, /* S#Acc,*/ B]): SkipList.Map[T, A, B] =
      HASkipList.Map.empty[T, A, B]

    def empty[T <: Exec[T], A, B](keyObserver: SkipList.KeyObserver[T, A] = NoKeyObserver)
                                 (implicit tx: T, ord: scala.Ordering[/*T,*/ A],
                                  keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A],
                                  valueSerializer: TxSerializer[T, /*S#Acc,*/ B]): SkipList.Map[T, A, B] =
      HASkipList.Map.empty[T, A, B](keyObserver = keyObserver)

    def read[T <: Exec[T], A, B](in: DataInput, tx: T,
                                 keyObserver: SkipList.KeyObserver[T, A] = NoKeyObserver)
                                (implicit acc: tx.Acc, ord: scala.Ordering[/*T,*/ A],
                                 keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A],
                                 valueSerializer: TxSerializer[T, /*S#Acc,*/ B]): SkipList.Map[T, A, B] =
      HASkipList.Map.read[T, A, B](in, tx, keyObserver)

    def serializer[T <: Exec[T], A, B](keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                      (implicit ord: scala.Ordering[/*T,*/ A],
                                       keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A],
                                       valueSerializer: TxSerializer[T, /*S#Acc,*/ B]): TxSerializer[T, /*S#Acc,*/ Map[T, A, B]] =
      new MapSer[T, A, B](keyObserver)

    private final class MapSer[T <: Exec[T], A, B](keyObserver: SkipList.KeyObserver[T, A])
                                                  (implicit ord: scala.Ordering[/*T,*/ A],
                                                   keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A],
                                                   valueSerializer: TxSerializer[T, /*S#Acc,*/ B])
      extends TxSerializer[T, /*S#Acc,*/ Map[T, A, B]] {
      
      override def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Map[T, A, B] = 
        Map.read[T, A, B](in, tx, keyObserver)

      override def write(list: Map[T, A, B], out: DataOutput): Unit = list.write(out)

      override def toString = "SkipList.Map.serializer"
    }
  }

  trait Set[T <: Exec[T], /* @spec(KeySpec) */ A] extends SkipList[T, A, A] {
    /** Inserts a new key into the set.
     *
     * @param   key  the key to insert
     * @return  `true` if the key was new to the set,
     *          `false` if a node with the given key already existed
     */
    def add(key: A)(implicit tx: T): Boolean

    /** Removes a key from the set.
     *
     * @param key  the key to remove
     * @return     `true` if the key was found and removed, `false` if it was not found
     */
    def remove(key: A)(implicit tx: T): Boolean
  }

  trait Map[T <: Exec[T], /* @spec(KeySpec) */ A, /* @spec(ValueSpec) */ B] extends SkipList[T, A, (A, B)] {

    def keysIterator  (implicit tx: T): Iterator[A]
    def valuesIterator(implicit tx: T): Iterator[B]

    /** Inserts a new entry into the map.
     *
     * @param   key    the entry's key to insert
     * @param   value  the entry's value to insert
     * @return  the previous value stored at the key, or `None` if the key was not in the map
     */
    def put(key: A, value: B)(implicit tx: T): Option[B]

    /** Removes an entry from the map.
     *
     * @param   key  the key to remove
     * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
     */
    def remove(key: A)(implicit tx: T): Option[B]

    /** Queries the value for a given key.
     *
     * @param key  the key to look for
     * @return     the value if it was found at the key, otherwise `None`
     */
    def get(key: A)(implicit tx: T): Option[B]

    def getOrElse[B1 >: B](key: A, default: => B1)(implicit tx: T): B1

    def getOrElseUpdate(key: A, op: => B)(implicit tx: T): B
  }
}

/*sealed*/ trait SkipList[T <: Exec[T], /* @spec(KeySpec) */ A, /* @spec(ValueSpec) */ E] extends Mutable[Ident[T] /*S#Id*/, T] {
  /** Searches for the Branch of a given key.
   *
   * @param   key   the key to search for
   * @return  `true` if the key is in the list, `false` otherwise
   */
  def contains(key: A)(implicit tx: T): Boolean

  /** Finds the entry with the largest key which is smaller than or equal to the search key.
   *
   * @param key  the search key
   * @return     the found entry, or `None` if there is no key smaller than or equal
   *             to the search key (e.g. the list is empty)
   */
  def floor(key: A)(implicit tx: T): Option[E]

  /** Finds the entry with the smallest key which is greater than or equal to the search key.
   *
   * @param key  the search key
   * @return     the found entry, or `None` if there is no key greater than or equal
   *             to the search key (e.g. the list is empty)
   */
  def ceil(key: A)(implicit tx: T): Option[E]

  /** Returns the first element. Throws an exception if the list is empty. */
  def head(implicit tx: T): E

  /** Returns the first element, or `None` if the list is empty. */
  def headOption(implicit tx: T): Option[E]

  /** Returns the last element. Throws an exception if the list is empty. */
  def last(implicit tx: T): E

  def firstKey(implicit tx: T): A
  def lastKey (implicit tx: T): A

  /** Returns the last element, or `None` if the list is empty. */
  def lastOption(implicit tx: T): Option[E]

  def toIndexedSeq(implicit tx: T): Vec[E]
  def toList      (implicit tx: T): List[E]
  def toSeq       (implicit tx: T): Seq[E]
  def toSet       (implicit tx: T): Set[E]

  def clear()(implicit tx: T): Unit

  /** Finds the nearest item equal or greater
   * than an unknown item from an isomorphic
   * set. The isomorphism is represented by
   * a comparison function which guides the
   * binary search.
   *
   * @param   ord   a function that guides the search.
   *                should return -1 if the argument is smaller
   *                than the search key, 0 if both are equivalent,
   *                or 1 if the argument is greater than the search key.
   *                E.g., using some mapping, the function could look
   *                like `mapping.apply(_).compare(queryKey)`
   *
   * @return  the nearest item, or the maximum item
   */
  def isomorphicQuery(ord: Ordered[T, A])(implicit tx: T): (E, Int)

  // ---- stuff lost from collection.mutable.Set ----

  def +=(entry: E)(implicit tx: T): this.type
  def -=(key: A)  (implicit tx: T): this.type

  def isEmpty (implicit tx: T): Boolean
  def nonEmpty(implicit tx: T): Boolean

  def iterator(implicit tx: T): Iterator[E]

  def debugPrint()(implicit tx: T): String

  def keySerializer: NewImmutSerializer[/*T, S#Acc,*/ A]

  /** The number of levels in the skip list. */
  def height(implicit tx: T): Int

  /** Reports the number of keys in the skip list (size of the bottom level).
   * This operation may take up to O(n) time, depending on the implementation.
   */
  def size(implicit tx: T): Int

  /** The ordering used for the keys of this list. */
  implicit def ordering: scala.Ordering[/*T,*/ A]

  /** The minimum gap within elements of each skip level. */
  def minGap: Int

  /** The maximum gap within elements of each skip level. */
  def maxGap: Int
}