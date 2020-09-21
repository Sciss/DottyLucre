/*
 *  TMap.scala
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

import de.sciss.lucre.impl.TMapImpl
import de.sciss.serial.DataInput

import scala.annotation.switch
import scala.reflect.ClassTag

object TMap extends Obj.Type {
  final val typeId = 24

  override def init(): Unit = ()  // this type is known in advance.

  object Key {
    implicit object Int extends Key[Int] {
      final val typeId  = 2 // IntObj.typeId
      def serializer: NewImmutSerializer[scala.Int] = NewImmutSerializer.Int
    }
    implicit object Long extends Key[Long] {
      final val typeId  = 3 // LongObj.typeId
      def serializer: NewImmutSerializer[scala.Long] = NewImmutSerializer.Long
    }
    implicit object String extends Key[String] {
      final val typeId  = 8 // StringObj.typeId
      def serializer: NewImmutSerializer[java.lang.String] = NewImmutSerializer.String
    }

    def apply(typeId: Int): Key[_] = (typeId: @switch) match {
      case Int   .`typeId` => Int
      case Long  .`typeId` => Long
      case String.`typeId` => String
    }
  }
  /** Cheesy little type class for supported immutable keys. */
  sealed trait Key[K] {
    def typeId: Int
    def serializer: NewImmutSerializer[K]
  }

  object Modifiable {
    def apply[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]()(implicit tx: T): Modifiable[T, K, Repr] =
      TMapImpl[T, K, Repr]()

    def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T)
                                                               (implicit acc: tx.Acc): Modifiable[T, K, Repr] =
      serializer[T, K, Repr].read(in, tx)

    implicit def serializer[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TSerializer[T, Modifiable[T, K, Repr]] =
      TMapImpl.modSerializer[T, K, Repr]
  }

  trait Modifiable[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]] extends TMap[T, K, Repr] {
    // override def copy()(implicit tx: T): Modifiable[T, K, Repr]

    /** Inserts a new entry into the map.
     *
     * @param  key  the key to insert
     * @param  value the value to store for the given key
     * @return the previous value stored at the key, or `None` if the key was not in the map
     */
    def put(key: K, value: V): Option[V]

    def +=(kv: (K, V)): this.type

    /** Removes an entry from the map.
     *
     * @param   key  the key to remove
     * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
     */
    def remove(key: K): Option[V]

    def -=(key: K): this.type
  }

  def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T)
                                                             (implicit acc: tx.Acc): TMap[T, K, Repr] =
    serializer[T, K, Repr].read(in, tx)

  def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] =
    TMapImpl.readIdentifiedObj(in, tx)

  implicit def serializer[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TSerializer[T, TMap[T, K, Repr]] =
    TMapImpl.serializer[T, K, Repr]

  final case class Update[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]](map: TMap[T, K, Repr],
                                                                        changes: List[Change[/*T,*/ K, Repr[T]]])
    extends TMapLike.Update[/*T,*/ K, Repr[T]]

  type Change[/*T <: Txn[T],*/ K, V] = TMapLike.Change[/*T,*/ K, V]

  type Added    [/*T <: Txn[T],*/ K, V] = TMapLike.Added  [/*T,*/ K, V]
  type Removed  [/*T <: Txn[T],*/ K, V] = TMapLike.Removed[/*T,*/ K, V]
  type Replaced [/*T <: Txn[T],*/ K, V] = TMapLike.Removed[/*T,*/ K, V]

  val  Added    : TMapLike.Added   .type = TMapLike.Added
  val  Removed  : TMapLike.Removed .type = TMapLike.Removed
  val  Replaced : TMapLike.Replaced.type = TMapLike.Replaced
}
trait TMap[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]]
  extends TMapLike[T, K, Repr[T]] with Obj[T] with Publisher[T, TMap.Update[T, K, Repr]] {

  type V = Repr[T]

  def modifiableOption: Option[TMap.Modifiable[T, K, Repr]]

  def iterator      : Iterator[(K, V)]
  def keysIterator  : Iterator[K]
  def valuesIterator: Iterator[V]

  def $[R[~ <: Txn[~]] <: Repr[~]](key: K)(implicit ct: ClassTag[R[T]]): Option[R[T]]
}