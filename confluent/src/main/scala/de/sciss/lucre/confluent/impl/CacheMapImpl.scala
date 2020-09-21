/*
 *  CacheMapImpl.scala
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
package impl

import de.sciss.lucre.TSerializer
import de.sciss.serial
import de.sciss.serial.ImmutableSerializer
import de.sciss.lucre.confluent.Log.log

import scala.concurrent.stm.{InTxn, TxnLocal}

object CacheMapImpl {
   /** Instances of `Entry` are stored for each variable write in a transaction. They
     * are flushed at the commit to the persistent store. There are two sub types, a
     * transactional and a non-transactional one. A non-transactional cache entry can de-serialize
     * the value without transactional context, e.g. this is true for all primitive types.
     * A transactional entry is backed by a `Serializer`. To be saved in the store which uses
     * a sub system (`Durable`), serialization is a two-step process, using an intermediate
     * binary representation.
     */
   sealed trait Entry[T <: Txn[T], -K, -Store] {
     def path: Access[T]

     def flush(key: K, outTerm: Long, store: Store)(implicit tx: T): Unit

     def value: Any
   }
}

/** A cache map puts an in-memory transaction local cache in front of a persistent store. Entries written
  * during the transaction are held in this cache for fast retrieval. But the cache serves a second purpose:
  * In the confluent system, the write paths are incomplete during the transaction, as it is not known in
  * advance whether a meld forces a new index tree to be generated or not. In this case, the implementation
  * needs to gather this information during the transaction, and when the flush is performed, the new
  * terminal version is appended before writing the cached entries to the persistent store.
  *
  * @tparam T   the underlying system's transaction type
  * @tparam K   the key type (typically `Int` for a variable map or `Long` for an identifier map)
  */
sealed trait CacheMapImpl[T <: Txn[T], K, Store]
  extends CacheMap[T, K, Store] {

  import CacheMapImpl._

  private val cache = TxnLocal(Map.empty[K, Map[Long, Entry[T, K, Store]]])

  // ---- implementation ----

  final def getCacheOnly[A](key: K, path: Access[T])(implicit tx: T): Option[A] =
    cache.get(tx.peer).get(key).flatMap(_.get(path.sum).map { e =>
      e.value.asInstanceOf[A]
    })

  final def cacheContains(key: K, path: Access[T])(implicit tx: T): Boolean =
    cache.get(tx.peer).get(key) match {
      case Some(map) => map.contains(path.sum)
      case _ => false
    }

  /** Removes an entry from the cache, and only the cache. This will not affect any
    * values also persisted to `persistent`! If the cache does not contain an entry
    * at the given `key`, this method simply returns.
    *
    * @param key        key at which the entry is stored
    * @param tx         the current transaction
    */
  final def removeCacheOnly(key: K, path: Access[T])(implicit tx: T): Boolean = {
    implicit val itx: InTxn = tx.peer
    //      cache.transform( _ - key )( tx.peer )
    val m = cache.get
    val km = m.getOrElse(key, Map.empty)
    val hash = path.sum
    if (km.contains(hash)) {
      val km1 = km - hash
      val m1 = if (km1.isEmpty) {
        m - key
      } else {
        m + (key -> km1)
      }
      cache.set(m1)
      true
    } else {
      false
    }
  }

  final protected def putCacheOnly(key: K, e: Entry[T, K, Store])(implicit tx: T): Unit = {
    implicit val itx: InTxn = tx.peer
    cache.transform { mapMap =>
      val mapOld = mapMap.getOrElse(key, Map.empty[Long, Entry[T, K, Store]])
      val mapNew = mapOld + (e.path.sum -> e)
      mapMap + ((key, mapNew))
    }
  }

  /** This method should be invoked from the implementations flush hook after it has
    * determined the terminal version at which the entries in the cache are written
    * to the persistent store. If this method is not called, the cache will just
    * vanish and not be written out to the `persistent` store.
    *
    * @param term    the new version to append to the paths in the cache (using the `PathLike`'s `addTerm` method)
    * @param tx      the current transaction (should be in commit or right-before commit phase)
    */
  final def flushCache(term: Long)(implicit tx: T): Unit = {
    val p = store
    cache.get(tx.peer).foreach { case (key, map) =>
      map.foreach { tup2 =>
        val e = tup2._2
        e.flush(key, term, p)
      }
    }
  }
}

object DurableCacheMapImpl {
  import CacheMapImpl.Entry

  type Store[T <: Txn[T], K] = DurablePersistentMap[T, K]

  def newIntCache[T <: Txn[T]](map: Store[T, Int]): CacheMap.Durable[T, Int, Store[T, Int]] =
    new DurableCacheMapImpl[T, Int] {
      // final protected def emptyCache: Map[Int, Any] = Map.empty

      final val store: Store[T, Int] = map
    }

  private final class NonTxnEntry[T <: Txn[T], K, A]
  (val path: Access[T], val value: A)(implicit serializer: ImmutableSerializer[A])
    extends Entry[T, K, Store[T, K]] {
    override def toString = s"NonTxnEntry($value)"

    def flush(key: K, outTerm: Long, store: Store[T, K])(implicit tx: T): Unit = {
      val pathOut = path.addTerm(outTerm)
      log(s"txn flush write $value for ${pathOut.mkString(s"<$key @ ", ",", ">")}")
      store.putImmutable(key, pathOut, value)
    }
  }

  private final class TxnEntry[T <: Txn[T], K, A](val path: Access[T], val value: A)
                                                 (implicit serializer: TSerializer[T, A])
    extends Entry[T, K, Store[T, K]] {
    
    override def toString = s"TxnEntry($value)"

    def flush(key: K, outTerm: Long, store: Store[T, K])(implicit tx: T): Unit = {
      val pathOut = path.addTerm(outTerm)
      log(s"txn flush write $value for ${pathOut.mkString(s"<$key @ ", ",", ">")}")
//      val out = DataOutput()
//      serializer.write(value, out)
//      val arr = out.toByteArray
      store.put(key, pathOut, value)
    }
  }
}

trait DurableCacheMapImpl[T <: Txn[T], K]
  extends CacheMap.Durable[T, K, DurablePersistentMap[T, K]]
  with CacheMapImpl[T, K, DurablePersistentMap[T, K]] {

  import DurableCacheMapImpl._

  // private[this] val readCache = TxnLocal(new java.util.WeakHashMap[(K, Access[T]), AnyRef])

  /** Stores an entry in the cache for which 'only' a transactional serializer exists.
    *
    * Note that the caller is responsible for monitoring this call, and if necessary installing
    * a before-commit handler which will call into the abstract method `flushCache()`.
    *
    * @param key        key at which the entry will be stored
    * @param path       write path when persisting
    * @param value      value to be stored (entry)
    * @param tx         the current transaction
    * @param serializer the serializer to use for the value
    * @tparam A         the type of value stored
    */
  final def putCacheTxn[A](key: K, path: Access[T], value: A)
                          (implicit tx: T, serializer: TSerializer[T, A]): Unit =
    putCacheOnly(key, new TxnEntry[T, K, A](path, value))

  /** Stores an entry in the cache for which a non-transactional serializer exists.
    *
    * Note that the caller is responsible for monitoring this call, and if necessary installing
    * a before-commit handler which will call into the abstract method `flushCache()`.
    *
    * @param key        key at which the entry will be stored
    * @param path       write path when persisting
    * @param value      value to be stored (entry)
    * @param tx         the current transaction
    * @param serializer the serializer to use for the value
    * @tparam A         the type of value stored
    */
  final def putCacheNonTxn[A](key: K, path: Access[T], value: A)
                             (implicit tx: T, serializer: ImmutableSerializer[A]): Unit =
    putCacheOnly(key, new NonTxnEntry[T, K, A](path, value))

  /** Retrieves a value from the cache _or_ the underlying store (if not found in the cache), where 'only'
    * a transactional serializer exists.
    *
    * If no value is found for the current path, this will try to read the most recent entry along the path.
    *
    * @param key        key at which the entry is stored
    * @param path       access path for the read
    * @param tx         the current transaction
    * @param serializer the serializer to use for the value
    * @tparam A         the type of value stored
    * @return           the most recent value found, or `None` if a value cannot be found for the given path,
    *                   neither in the cache nor in the persistent store.
    */
  final def getCacheTxn[A](key: K, path: Access[T])
                          (implicit tx: T, serializer: TSerializer[T, A]): Option[A] = {
    val v1Opt = getCacheOnly(key, path)
    if (v1Opt.isDefined) return v1Opt

    // val rc    = readCache.get(tx.peer)
    // val kp    = (key, path)
    // val ref_? = rc.get(kp)
    // val v2_?  = if (ref_? == null) null else ref_?.asInstanceOf[WeakReference[_]].get()
    // if (v2_? == null) {
      /* val v3Opt = */ store.get[A](key, path)
    // if (v3Opt.isDefined) rc.put(kp, new WeakReference(v3Opt.get))
    //   v3Opt
    // } else {
    //   Some(v2_?.asInstanceOf[A])
    // }
  }

  /** Retrieves a value from the cache _or_ the underlying store (if not found in the cache), where a
    * non-transactional serializer exists.
    *
    * If no value is found for the current path, this will try to read the most recent entry along the path.
    *
    * @param key        key at which the entry is stored
    * @param path       access path for the read
    * @param tx         the current transaction
    * @param serializer the serializer to use for the value
    * @tparam A         the type of value stored
    * @return           the most recent value found, or `None` if a value cannot be found for the given path,
    *                   neither in the cache nor in the persistent store.
    */
  final def getCacheNonTxn[A](key: K, path: Access[T])
                             (implicit tx: T, serializer: ImmutableSerializer[A]): Option[A] = {
    val v1Opt = getCacheOnly(key, path)
    if (v1Opt.isDefined) return v1Opt

    // val rc    = readCache.get(tx.peer)
    // val kp    = (key, path)
    // val v2_?  = rc.get(kp)
    // if (v2_? == null) {
      /* val v3Opt = */ store.getImmutable[A](key, path)
    // if (v3Opt.isDefined) rc.put(kp, v3Opt.get.asInstanceOf[AnyRef])
    // v3Opt
    // } else {
    //   Some(v2_?.asInstanceOf[A])
    // }
  }

  //   final protected def isFresh( key: K, path: Access[T] )( implicit tx: T ) : Boolean =
  //      cacheContains( key, path ) || {
  //         store.getWithSuffix()
  //      }

  final def removeCache(key: K, path: Access[T])(implicit tx: T): Boolean =
    removeCacheOnly(key, path) || store.remove(key, path)
}

object InMemoryCacheMapImpl {
  private type Store[T <: Txn[T], K] = InMemoryConfluentMap[T, K]

  private final class Entry[T <: Txn[T], K, A]
  (val path: Access[T], val value: A)
    extends CacheMapImpl.Entry[T, K, Store[T, K]] {
    override def toString = s"Entry($value)"

    def flush(key: K, outTerm: Long, store: Store[T, K])(implicit tx: T): Unit = {
      val pathOut = path.addTerm(outTerm)
      log(s"txn flush write $value for ${pathOut.mkString(s"<$key @ ", ",", ">")}")
      store.put(key, pathOut, value)
    }
  }
}

trait InMemoryCacheMapImpl[T <: Txn[T], /* @spec(KeySpec) */ K]
  extends CacheMap.InMemory[T, K, InMemoryConfluentMap[T, K]]
  with CacheMapImpl[T, K, InMemoryConfluentMap[T, K]] {

  import InMemoryCacheMapImpl._

  final def putCache[/* @spec(ValueSpec) */ A](key: K, path: Access[T], value: A)(implicit tx: T): Unit =
    putCacheOnly(key, new Entry(path, value))

  final def getCache[A](key: K, path: Access[T])(implicit tx: T): Option[A] =
    getCacheOnly(key, path).orElse(store.get[A](key, path))

  final def removeCache(key: K, path: Access[T])(implicit tx: T): Boolean = {
    removeCacheOnly(key, path) || store.remove(key, path)
  }
}

// ---------------------------------------------------------------------

object PartialCacheMapImpl {
  import CacheMapImpl.Entry

  private type Store[T <: Txn[T], K] = DurablePersistentMap[T, K]

  def newIntCache[T <: Txn[T]](map: Store[T, Int]): CacheMap.Partial[T, Int, Store[T, Int]] =
    new PartialCacheMapImpl[T, Int] {

      // final protected def emptyCache: Map[Int, Any] = Map.empty
      final val store: DurablePersistentMap[T, Int] = map
    }

  private final class PartialEntry[T <: Txn[T], K, A](val fullPath: Access[T], val value: A)
                                                     (implicit serializer: TSerializer[T, A])
    extends Entry[T, K, Store[T, K]] {

    override def toString = s"PartialEntry($value)"

    val path: Access[T] = fullPath.partial

    def flush(key: K, outTerm: Long, store: Store[T, K])(implicit tx: T): Unit = {
      val pathOut = fullPath.addTerm(outTerm)
      log(s"txn flush write $value for ${pathOut.mkString(s"<$key @ ", ",", ">")}")
//      val out     = DataOutput()
//      serializer.write(value, out)
//      val arr     = out.toByteArray
      store.put(key, pathOut, value)
    }
  }
}

trait PartialCacheMapImpl[T <: Txn[T], K]
  extends CacheMap.Partial[T, K, DurablePersistentMap[T, K]]
  with CacheMapImpl       [T, K, DurablePersistentMap[T, K]] {

  import PartialCacheMapImpl._

  final def putPartial[A](key: K, path: Access[T], value: A)(implicit tx: T, serializer: TSerializer[T, A]): Unit =
    putCacheOnly(key, new PartialEntry[T, K, A](path, value))

  final def getPartial[A](key: K, path: Access[T])(implicit tx: T, serializer: TSerializer[T, A]): Option[A] =
    getCacheOnly(key, path.partial) orElse store.get[A](key, path)

  final def removeCache(key: K, path: Access[T])(implicit tx: T): Boolean =
    removeCacheOnly(key, path) || store.remove(key, path)
}