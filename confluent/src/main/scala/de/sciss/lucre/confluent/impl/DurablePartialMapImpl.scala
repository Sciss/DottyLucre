/*
 *  DurablePartialMapImpl.scala
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

import de.sciss.lucre.DataStore
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}

import scala.annotation.switch

object DurablePartialMapImpl {
  private sealed trait Entry[T, A]
  private final case class EntryPre   [T, A](hash: Long)        extends Entry[T, A]
  // XXX TODO: `term` is unused ??
  private final case class EntrySingle[T, A](term: Long, v: A)  extends Entry[T, A]
  private final case class EntryMap   [T, A](m: IndexMap[T, A]) extends Entry[T, A]
}

sealed trait DurablePartialMapImpl[T <: Txn[T], K] extends DurablePersistentMap[T, K] {
  import DurablePartialMapImpl._

  protected def store: DataStore

  protected def handler: PartialMapHandler[T]

  protected def writeKey(key: K, out: DataOutput): Unit

  final def isFresh(key: K, tx: T)(implicit conPath: tx.Acc): Boolean = true // III

  final def putImmutable[A](key: K, value: A, tx: T)
                           (implicit conPath: tx.Acc, format: ConstFormat[A]): Unit = {
    val term = conPath.term
    // first we need to see if anything has already been written to the index of the write path
    val eOpt: Option[Entry[T, A]] = store.flatGet[Entry[T, A]]({ out =>
      out.writeByte(2)
      writeKey(key, out)
    })({ in =>
      (in.readByte(): @switch) match {
        case 1 =>
          // a single 'root' value is found. extract it for successive re-write.
          val term2 = in.readLong()
          val prev  = format.read(in)
          Some(EntrySingle(term2, prev))
        case 2 =>
          // there is already a map found
          val m = handler.readPartialMap[A](/* index, */ in)(tx, format)
          Some(EntryMap(m))
        case _ => None // this would be a partial hash which we don't use
      }
    })(tx)
    
    eOpt match {
      // with the previous entry read, react as follows:
      // if there is a single entry, construct a new ancestor.map with the
      // entry's value taken as root value
      case Some(EntrySingle(_, prevValue)) =>
        putFullMap[A](key, /* index, */ term, value, /* prevTerm, */ prevValue)(tx, format)
      // if there is an existing map, simply add the new value to it
      case Some(EntryMap(m)) =>
        //if( key == 0 ) {
        //   println( "::::: adding to existing map " + m )
        //}
        m.add(term, value)(tx)
      // if there is no previous entry...
      case _ =>
        // we may write a single entry if and only if the value can be seen
        // as a root value (the write path corresponds to the construction
        // of the entity, e.g. path == <term, term>; or the entity was
        // re-written in the tree root, hence path.suffix == <term, term>)
        //            val indexTerm = index.term
        //            if( term == indexTerm ) {
        //               putPartials( key, index )
        putFullSingle[A](key, /* index, */ term, value)(tx, format)
    }
  }
  
  final def put[A](key: K, value: A, tx: T)(implicit conPath: tx.Acc, format: TFormat[T, A]): Unit = {
    ???
  }

  private def putFullMap[A](key: K, /* conIndex: Access[T], */ term: Long, value: A, /* prevTerm: Long, */
                            prevValue: A)(implicit tx: T, format: ConstFormat[A]): Unit = {
    // create new map with previous value
    val m = handler.newPartialMap[A](/* conIndex, prevTerm, */ prevValue)

    store.put { out =>
      out.writeByte(2)
      writeKey(key, out) // out.writeInt( key )
      //         out.writeLong( index.sum )
    } { out =>
      out.writeByte(2) // aka map entry
      m.write(out)
    }
    // then add the new value
    m.add(term, value)
  }

  // store the full value at the full hash (path.sum)
  private def putFullSingle[/* @spec(ValueSpec) */ A](key: K, /* conIndex: Access[T], */ term: Long, value: A)
                                               (implicit tx: T, format: ConstFormat[A]): Unit =
    store.put { out =>
      out.writeByte(2)
      writeKey(key, out) // out.writeInt( key )
      //         out.writeLong( index.sum )
    } { out =>
      out.writeByte(1) // aka entry single
      out.writeLong(term)
      format.write(value, out)
    }

  def remove(key: K, tx: T)(implicit path: tx.Acc): Boolean = {
    println("Durable partial map : remove not yet implemented")
    true
  }

  final def getImmutable[A](key: K, tx: T)(implicit conPath: tx.Acc, format: ConstFormat[A]): Option[A] = {
    if (conPath.isEmpty) return None
    val (maxIndex, maxTerm) = conPath.splitIndex
    getWithPrefixLen[A, A](key, maxIndex, maxTerm)((/* _, */ _, value) => value)(tx, format)
  }

  final def get[A](key: K, tx: T)(implicit conPath: tx.Acc, format: TFormat[T, A]): Option[A] = {
    if (conPath.isEmpty) return None
    val (maxIndex, maxTerm) = conPath.splitIndex
    getWithPrefixLen[Array[Byte], A](key, maxIndex, maxTerm) {
      (/* preLen, */ writeTerm, arr) =>
        val treeTerm = handler.getIndexTreeTerm(writeTerm)(tx)
        val i = maxIndex.maxPrefixLength(treeTerm) // XXX TODO: this is wrong -- maxPrefixLength currently compares sums not terms
        // XXX TODO ugly ugly ugly
        val suffix = if (i == 0) {
          val inPath = tx.inputAccess
          val j = inPath.maxPrefixLength(treeTerm) // XXX TODO: this is wrong -- maxPrefixLength currently compares sums not terms
          require(j > 0)
          inPath.drop(j)
        } else {
          conPath.drop(i)
        }
        val access  = writeTerm +: suffix
        val in      = DataInput(arr)
        tx.withReadAccess(access)(format.readT(in)(tx))
    } (tx, ByteArrayFormat)
  }

  // XXX boom! specialized
  private def getWithPrefixLen[A, B](key: K, maxConIndex: Access[T], maxTerm: Long)
                                    (fun: ( /* Int, */ Long, A) => B)
                                    (implicit tx: T, format: ConstFormat[A]): Option[B] = {
   store.flatGet { out =>
      out.writeByte(2)
      writeKey(key, out) // out.writeInt( key )
      //         out.writeLong( preSum )
    } { in =>
      in.readByte() /*: @switch */ match {
        //            case 0 => // partial hash
        //               val hash = in.readLong()
        //               //                  EntryPre[ S ]( hash )
        //               val idx     = maxIndex.indexOfSum( hash )
        //               val idxCon  = idx << 1  // III XXX TODO check correctness
        //               val (conIndex, fullTerm) = maxConIndex.splitAtIndex( idxCon )
        //               getWithPrefixLen( key, conIndex, fullTerm )( fun )

        case 1 =>
          // --- THOUGHT: This assertion is wrong. We need to replace store.get by store.flatGet.
          // if the terms match, we have Some result. If not, we need to ask the index tree if
          // term2 is ancestor of term. If so, we have Some result, if not we have None.
          //                  assert( term == term2, "Accessed version " + term.toInt + " but found " + term2.toInt )

          // --- ADDENDUM: I believe we do not need to store `term2` at all, it simply doesn't
          // matter. Given a correct variable system, there is no notion of uninitialised values.
          // Therefore, we cannot end up in this case without the previous stored value being
          // correctly the nearest ancestor of the search term. For example, say the index tree
          // is v0, and the variable was created in v2. Then there is no way that we try to
          // read that variable with v0. The value stored here is always the initialisation.
          // If there was a second assignment for the same index tree, we'd have found an
          // entry map, and we can safely _coerce_ the previous value to be the map's
          // _root_ value.

          val term2 = in.readLong()
          val value = format.read(in)
          //                  EntrySingle[ S, A ]( term2, value )
          Some(fun(/* preConLen, */ term2, value))

        case 2 =>
          val m = handler.readPartialMap[A](/* maxConIndex, */ in)
          val inTerm = tx.inputAccess.term
          val (term2, value) = m.nearest(inTerm)
          Some(fun(term2, value))
      }
    }
  }
}

final class PartialIntMapImpl[T <: Txn[T]](protected val store: DataStore, protected val handler: PartialMapHandler[T])
  extends DurablePartialMapImpl[T, Int] {

  protected def writeKey(key: Int, out: DataOutput): Unit = out.writeInt(key)
}

final class PartialLongMapImpl[T <: Txn[T]](protected val store: DataStore, protected val handler: PartialMapHandler[T])
  extends DurablePartialMapImpl[T, Long] {

  protected def writeKey(key: Long, out: DataOutput): Unit = out.writeLong(key)
}
