/*
 *  DurablePersistentMap.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.confluent.impl.{ConfluentIntMapImpl, ConfluentLongMapImpl, PartialIntMapImpl}
import de.sciss.lucre.{DataStore, ConstantSerializer, TSerializer}

object DurablePersistentMap {
  def newConfluentIntMap[T <: Txn[T]](store: DataStore, handler: IndexMapHandler[T],
                                      isOblivious: Boolean): DurablePersistentMap[T, Int] =
    new ConfluentIntMapImpl[T](store, handler, isOblivious)

  def newConfluentLongMap[T <: Txn[T]](store: DataStore, handler: IndexMapHandler[T],
                                       isOblivious: Boolean): DurablePersistentMap[T, Long] =
    new ConfluentLongMapImpl[T](store, handler, isOblivious)

  def newPartialMap[T <: Txn[T]](store: DataStore, handler: PartialMapHandler[T]): DurablePersistentMap[T, Int] =
    new PartialIntMapImpl[T](store, handler)
}

/** Interface for a confluently or partially persistent storing key value map. Keys (type `K`) might
  * be single object identifiers (as the variable storage case), or combined keys
  * (as in the live map case).
  *
  * @tparam T   the underlying system's transaction type
  * @tparam K   the key type
  */
trait DurablePersistentMap[T <: Txn[T], /* @spec(KeySpec) */ K] {
  /** Stores a new value for a given write path.
    *
    * The serializer given is _non_transactional. This is because this trait bridges confluent
    * and ephemeral world (it may use a durable backend, but the data structures used for
    * storing the confluent graph are themselves ephemeral). If the value `A` requires a
    * transactional serialization, the current approach is to pre-serialize the value into
    * an appropriate format (e.g. a byte array) before calling into `put`. In that case
    * the wrapping structure must be de-serialized after calling `get`.
    *
    * @param key        the identifier for the object
    * @param path       the path through which the object has been accessed (the version at which it is read)
    * @param value      the value to store
    * @param tx         the transaction within which the access is performed
    * @param serializer the serializer used to store the entity's values
    * @tparam A         the type of values stored with the entity
    */
  def putImmutable[A](key: K, value: A, tx: T)(implicit path: tx.Acc, serializer: ConstantSerializer[A]): Unit

  def put[A](key: K, value: A, tx: T)(implicit path: tx.Acc, serializer: TSerializer[T, A]): Unit

  /** Finds the most recent value for an entity `id` with respect to version `path`.
    *
    * The serializer given is _non_transactional. This is because this trait bridges confluent
    * and ephemeral world (it may use a durable backend, but the data structures used for
    * storing the confluent graph are themselves ephemeral). If the value `A` requires a
    * transactional serialization, the current approach is to pre-serialize the value into
    * an appropriate format (e.g. a byte array) before calling into `put`. In that case
    * the wrapping structure must be de-serialized after calling `get`.
    *
    * @param key        the identifier for the object
    * @param path       the path through which the object has been accessed (the version at which it is read)
    * @param tx         the transaction within which the access is performed
    * @param serializer the serializer used to store the entity's values
    * @tparam A         the type of values stored with the entity
    * @return           `None` if no value was found, otherwise a `Some` of that value.
    */
  def getImmutable[A](key: K, tx: T)(implicit path: tx.Acc, serializer: ConstantSerializer[A]): Option[A]

  /** Finds the most recent value for an entity `id` with respect to version `path`. If a value is found,
    * it is return along with a suffix suitable for identifier path actualisation.
    *
    * @param key        the identifier for the object
    * @param path       the path through which the object has been accessed (the version at which it is read)
    * @param tx         the transaction within which the access is performed
    * @param serializer the serializer used to store the entity's values
    * @tparam A         the type of values stored with the entity
    * @return           `None` if no value was found, otherwise a `Some` of the tuple consisting of the
    *                   suffix and the value. The suffix is the access path minus the prefix at which the
    *                   value was found. However, the suffix overlaps the prefix in that it begins with the
    *                   tree entering/exiting tuple at which the value was found.
    */
  def get[A](key: K, tx: T)(implicit path: tx.Acc, serializer: TSerializer[T, A]): Option[A]

  /** '''Note:''' requires that `path` is non-empty. */
  def isFresh(key: K, tx: T)(implicit path: tx.Acc): Boolean

  def remove(key: K, tx: T)(implicit path: tx.Acc): Boolean
}
