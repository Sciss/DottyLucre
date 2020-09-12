/*
 *  MapLike.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.event.Observable

object MapLike {
  trait Update[T <: Txn[T], K, Repr[~ <: Txn[~]]] {
    def changes: scala.List[Change[T, K, Repr[T]]]
  }

  sealed trait Change[T <: Txn[T], K, V] {
    def key  : K
    def value: V
  }

  final case class Added   [T <: Txn[T], K, V](key: K, value: V) extends Change[T, K, V]
  final case class Removed [T <: Txn[T], K, V](key: K, value: V) extends Change[T, K, V]
  final case class Replaced[T <: Txn[T], K, V](key: K, before: V, now: V) extends Change[T, K, V] {
    def value: V = now
  }
}
// XXX TODO why scalac does not let us use `Base` (problem in evt.Map)?
trait MapLike[T <: Txn[T], K, Repr[~ <: Txn[~]] /*<: Form[~]*/] extends Disposable[T] {

  type V = Repr[T]

  def isEmpty (implicit tx: T): Boolean
  def nonEmpty(implicit tx: T): Boolean

  def changed: Observable[T, MapLike.Update[T, K, Repr]]

  /** Searches for the map for a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the map, `false` otherwise
    */
  def contains(key: K)(implicit tx: T): Boolean

  /** Queries the value for a given key.
    *
    * @param key  the key to look for
    * @return     the value if it was found at the key, otherwise `None`
    */
  def get(key: K)(implicit tx: T): Option[V]
}