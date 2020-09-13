/*
 *  TMapLike.scala
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

import TMapLike.Update

object TMapLike {
  trait Update[/*T <: Txn[T],*/ K, V /*Repr[~ <: Txn[~]]*/] {
    def changes: scala.List[Change[/*T,*/ K, V /*Repr[T]*/]]
  }

  sealed trait Change[/*T <: Txn[T],*/ K, V] {
    def key  : K
    def value: V
  }

  final case class Added   [/*T <: Txn[T],*/ K, V](key: K, value: V) extends Change[/*T,*/ K, V]
  final case class Removed [/*T <: Txn[T],*/ K, V](key: K, value: V) extends Change[/*T,*/ K, V]
  final case class Replaced[/*T <: Txn[T],*/ K, V](key: K, before: V, now: V) extends Change[/*T,*/ K, V] {
    def value: V = now
  }
}
trait TMapLike[T <: Txn[T], K, V /*Repr[~ <: Txn[~]]*/] extends Disposable {

//  type V = Repr[T]

  def isEmpty : Boolean
  def nonEmpty: Boolean

  def changed: Observable[T, Update[/*T,*/ K, V /*Repr*/]]

  /** Searches for the map for a given key.
   *
   * @param   key   the key to search for
   * @return  `true` if the key is in the map, `false` otherwise
   */
  def contains(key: K): Boolean

  /** Queries the value for a given key.
   *
   * @param key  the key to look for
   * @return     the value if it was found at the key, otherwise `None`
   */
  def get(key: K): Option[V]
}