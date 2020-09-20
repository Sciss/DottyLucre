///*
// *  Map.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.event
//
//import de.sciss.lucre.event.impl.{MapImpl => Impl}
//import de.sciss.lucre.stm.{Elem, Form, MapLike, NewImmutSerializer, Obj, TxSerializer, Txn}
//import de.sciss.serial.{DataInput, Serializer}
//
//import scala.annotation.switch
//import scala.reflect.ClassTag
//
//object Map extends Obj.Type {
//  final val typeId = 24
//
//  override def init(): Unit = ()  // this type is known in advance.
//
//  object Key {
//    implicit object Int extends Key[Int] {
//      final val typeId  = 2 // IntObj.typeId
//      def serializer: NewImmutSerializer[scala.Int] = NewImmutSerializer.Int
//    }
//    implicit object Long extends Key[Long] {
//      final val typeId  = 3 // LongObj.typeId
//      def serializer: NewImmutSerializer[scala.Long] = NewImmutSerializer.Long
//    }
//    implicit object String extends Key[String] {
//      final val typeId  = 8 // StringObj.typeId
//      def serializer: NewImmutSerializer[java.lang.String] = NewImmutSerializer.String
//    }
//    
//    def apply(typeId: Int): Key[_] = (typeId: @switch) match {
//      case Int   .`typeId` => Int
//      case Long  .`typeId` => Long
//      case String.`typeId` => String
//    }
//  }
//  /** Cheesy little type class for supported immutable keys. */ 
//  sealed trait Key[K] {
//    def typeId: Int
//    def serializer: NewImmutSerializer[K]
//  }
//
//  object Modifiable {
//    def apply[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, K, Repr] =
//      Impl[T, K, Repr]
//
//    def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T)
//                                                               (implicit acc: tx.Acc): Modifiable[T, K, Repr] =
//      serializer[T, K, Repr].read(in, tx)
//
//    implicit def serializer[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TxSerializer[T, Modifiable[T, K, Repr]] =
//      Impl.modSerializer[T, K, Repr]
//  }
//
//  trait Modifiable[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]] extends Map[T, K, Repr] {
//    // override def copy()(implicit tx: T): Modifiable[T, K, Repr]
//
//    /** Inserts a new entry into the map.
//      *
//      * @param  key  the key to insert
//      * @param  value the value to store for the given key
//      * @return the previous value stored at the key, or `None` if the key was not in the map
//      */
//    def put(key: K, value: V)(implicit tx: T): Option[V]
//
//    def +=(kv: (K, V))(implicit tx: T): this.type
//
//    /** Removes an entry from the map.
//      *
//      * @param   key  the key to remove
//      * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
//      */
//    def remove(key: K)(implicit tx: T): Option[V]
//
//    def -=(key: K)(implicit tx: T): this.type
//  }
//
//  def read[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T)
//                                       (implicit acc: tx.Acc): Map[T, K, Repr] =
//    serializer[T, K, Repr].read(in, tx)
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] =
//    Impl.readIdentifiedObj(in, tx)
//
//  implicit def serializer[T <: Txn[T], K: Key, Repr[~ <: Txn[~]] <: Elem[~]]: TxSerializer[T, Map[T, K, Repr]] =
//    Impl.serializer[T, K, Repr]
//
//  final case class Update[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]](map: Map[T, K, Repr],
//                                                                        changes: List[Change[T, K, Repr[T]]])
//    extends MapLike.Update[T, K, Repr]
//
//  type Change[T <: Txn[T], K, V] = MapLike.Change[T, K, V]
//  
//  type Added    [T <: Txn[T], K, V]     = MapLike.Added[T, K, V]
//  type Removed  [T <: Txn[T], K, V]     = MapLike.Removed[T, K, V]
//  type Replaced [T <: Txn[T], K, V]     = MapLike.Removed[T, K, V]
//
//  val  Added    : MapLike.Added   .type = MapLike.Added
//  val  Removed  : MapLike.Removed .type = MapLike.Removed
//  val  Replaced : MapLike.Replaced.type = MapLike.Replaced
//}
//trait Map[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Form[~]]
//  extends MapLike[T, K, Repr] with Obj[T] with Publisher[T, Map.Update[T, K, Repr]] {
//
////  type V = Repr[T]
//
//  def modifiableOption: Option[Map.Modifiable[T, K, Repr]]
//
//  def iterator      (implicit tx: T): Iterator[(K, V)]
//  def keysIterator  (implicit tx: T): Iterator[K]
//  def valuesIterator(implicit tx: T): Iterator[V]
//
//  def $[R[~ <: Txn[~]] <: Repr[~]](key: K)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]]
//}