/*
 *  MapObj.scala
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

import de.sciss.lucre.impl.TMapImpl
import de.sciss.serial.{ConstFormat, DataInput, TFormat}

import scala.annotation.switch
import scala.reflect.ClassTag

object MapObj extends Obj.Type {
  final val typeId = 24

  override def init(): Unit = ()  // this type is known in advance.

  object Key {
    implicit object Int extends Key[Int] {
      final val typeId  = 2 // IntObj.typeId
      def format: ConstFormat[scala.Int] = TFormat.Int
    }
    implicit object Long extends Key[Long] {
      final val typeId  = 3 // LongObj.typeId
      def format: ConstFormat[scala.Long] = TFormat.Long
    }
    implicit object String extends Key[String] {
      final val typeId  = 8 // StringObj.typeId
      def format: ConstFormat[java.lang.String] = TFormat.String
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
    def format: ConstFormat[K]
  }

  object Modifiable {
    def apply[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]()(implicit tx: T): Modifiable[T, K, Repr] =
      TMapImpl[T, K, Repr]()

    def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput)(implicit tx: T): Modifiable[T, K, Repr] =
      format[T, K, Repr].readT(in)

    implicit def format[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TFormat[T, Modifiable[T, K, Repr]] =
      TMapImpl.modFormat[T, K, Repr]
  }

  trait Modifiable[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]] extends MapObj[T, K, Repr] {
    // override def copy()(implicit tx: T): Modifiable[T, K, Repr]

    /** Inserts a new entry into the map.
     *
     * @param  key  the key to insert
     * @param  value the value to store for the given key
     * @return the previous value stored at the key, or `None` if the key was not in the map
     */
    def put(key: K, value: V)(implicit tx: T): Option[V]

    def +=(kv: (K, V))(implicit tx: T): this.type

    /** Removes an entry from the map.
     *
     * @param   key  the key to remove
     * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
     */
    def remove(key: K)(implicit tx: T): Option[V]

    def -=(key: K)(implicit tx: T): this.type
  }

  def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput)(implicit tx: T): MapObj[T, K, Repr] =
    format[T, K, Repr].readT(in)

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    TMapImpl.readIdentifiedObj(in)

  implicit def format[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TFormat[T, MapObj[T, K, Repr]] =
    TMapImpl.format[T, K, Repr]

  final case class Update[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]](map: MapObj[T, K, Repr],
                                                                        changes: List[Change[/*T,*/ K, Repr[T]]])
    extends MapObjLike.Update[K, Repr[T]]

  type Change[K, V] = MapObjLike.Change[K, V]

  type Added    [K, V] = MapObjLike.Added  [K, V]
  type Removed  [K, V] = MapObjLike.Removed[K, V]
  type Replaced [K, V] = MapObjLike.Removed[K, V]

  val  Added    : MapObjLike.Added   .type = MapObjLike.Added
  val  Removed  : MapObjLike.Removed .type = MapObjLike.Removed
  val  Replaced : MapObjLike.Replaced.type = MapObjLike.Replaced
}
trait MapObj[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]]
  extends MapObjLike[T, K, Repr[T]] with Obj[T] with Publisher[T, MapObj.Update[T, K, Repr]] {

  type V = Repr[T]

  def modifiableOption: Option[MapObj.Modifiable[T, K, Repr]]

  def iterator      (implicit tx: T): Iterator[(K, V)]
  def keysIterator  (implicit tx: T): Iterator[K]
  def valuesIterator(implicit tx: T): Iterator[V]

  def $[R[~ <: Txn[~]] <: Repr[~]](key: K)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]]
}