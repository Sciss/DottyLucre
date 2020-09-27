/*
 *  InMemoryConfluentMapImpl.scala
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

package de.sciss.lucre.confluent
package impl

import scala.collection.immutable.LongMap
import scala.concurrent.stm.{InTxn, TMap}

object InMemoryConfluentMapImpl {
  private trait Entry[+A]
  private final case class EntryPre    (hash: Long)       extends Entry[Nothing]
  private final case class EntryFull[A](term: Long, v: A) extends Entry[A]
}
final class InMemoryConfluentMapImpl[T <: Txn[T], K] extends InMemoryConfluentMap[T, K] {
  import InMemoryConfluentMapImpl._

  private type Entries = Map[Long, Entry[Any]]
  private val store = TMap.empty[K, Entries]

  override def toString = s"InMemoryConfluentMap($store)"

  def put[A](key: K, value: A, tx: T)(implicit path: tx.Acc): Unit = {
    implicit val itx: InTxn = tx.peer
    val (index, term) = path.splitIndex

    var entries = store.get(key).getOrElse(LongMap.empty)
    Hashing.foreachPrefix(index, entries.contains) {
      // for each key which is the partial sum, we store preSum which is the longest prefix of \tau' in \Pi
      case (hash, preSum) => entries += (hash -> EntryPre(preSum))
    }
    // then store full value
    entries += (index.sum -> EntryFull(term, value))
    store.put(key, entries)
  }

  def remove(key: K, tx: T)(implicit path: tx.Acc): Boolean = {
    implicit val itx: InTxn = tx.peer
    store.get(key) match {
      case Some(entries) =>
        val index       = path.index
        var newEntries  = entries
        Hashing.foreachPrefix(index, entries.contains) {
          case (hash, _) => newEntries -= hash
        }
        val indexSum  = index.sum
        val res       = newEntries.contains(indexSum)
        if (res) newEntries -= indexSum
        if (newEntries.isEmpty) {
          store.remove(key)
        } else {
          store.put(key, newEntries)
        }
        res

      case _ => false
    }
  }

  def get[A](key: K, tx: T)(implicit path: tx.Acc): Option[A] = {
    if (path.isEmpty) return None
    store.get(key)(tx.peer).flatMap { entries =>
      val (maxIndex, maxTerm) = path.splitIndex
      getWithPrefixLen[A, A](maxIndex, maxTerm, entries)((_, _, value) => value)
    }
  }

  def getWithSuffix[A](key: K, tx: T)(implicit path: tx.Acc): Option[(Access[T], A)] = {
    if (path.isEmpty) return None
    store.get(key)(tx.peer).flatMap { entries =>
      val (maxIndex, maxTerm) = path.splitIndex
      getWithPrefixLen[A, (Access[T], A)](maxIndex, maxTerm, entries)((preLen, writeTerm, value) =>
        (writeTerm +: path.drop(preLen), value)
      )
    }
  }

  private def getWithPrefixLen[A, B](maxIndex: Access[T], maxTerm: Long, entries: Entries)
                                    (fun: (Int, Long, A) => B): Option[B] = {

    val preLen  = Hashing.maxPrefixLength(maxIndex, entries.contains)
    val index   = if (preLen == maxIndex.size) {
      // maximum prefix lies in last tree
      maxIndex
    } else {
      // prefix lies in other tree
      maxIndex.take(preLen)
    }
    val preSum = index.sum
    entries.get(preSum).flatMap {
      case EntryPre(hash) => // partial hash
        val (fullIndex, fullTerm) = maxIndex.splitAtSum(hash)
        getWithPrefixLen(fullIndex, fullTerm, entries)(fun)

      case EntryFull(term2, value) => Some(fun(preLen, term2, value.asInstanceOf[A]))
    }
  }
}
