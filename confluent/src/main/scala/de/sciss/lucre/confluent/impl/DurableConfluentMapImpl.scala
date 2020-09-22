/*
 *  DurableConfluentMapImpl.scala
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

import de.sciss.lucre.{DataStore, ConstantSerializer, TSerializer}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

object DurableConfluentMapImpl {
  private sealed trait     Entry      [T, A]
  private final case class EntryPre   [T, A](hash: Long)        extends Entry[T, A]
  private final case class EntrySingle[T, A](term: Long, v: A)  extends Entry[T, A]
  private final case class EntryMap   [T, A](m: IndexMap[T, A]) extends Entry[T, A]

  private final class WithPrefix[A](val len: Int, val term: Long, val value: A)
}

sealed trait DurableConfluentMapImpl[T <: Txn[T], K] extends DurablePersistentMap[T, K] {
  import DurableConfluentMapImpl._

  protected def store: DataStore
  protected def handler: IndexMapHandler[T]
  protected def isOblivious: Boolean

  protected def writeKey(key: K, out: DataOutput): Unit

  final def isFresh(key: K, tx: T)(implicit path: tx.Acc): Boolean = {
    store.get({ out =>
      writeKey(key, out)
      out.writeLong(path.indexSum)
    })({ in =>
      (in.readByte(): @switch) match {
        case 1 => true // a single value is found
        case 2 => true // a map is found
        case _ => false // only a partial hash is found
      }
    })(tx).contains(true) // getOrElse(false)
  }

  final def putImmutable[A](key: K, value: A, tx: T)
                           (implicit path: tx.Acc, ser: ConstantSerializer[A]): Unit = {
    val (index, term) = path.splitIndex
    // first we need to see if anything has already been written to the index of the write path
    val eOpt: Option[Entry[T, A]] = store.flatGet[Entry[T, A]]({ out =>
      writeKey(key, out)
      out.writeLong(index.sum)
    })({ in =>
      (in.readByte(): @switch) match {
        case 1 =>
          // a single 'root' value is found. extract it for successive re-write.
          val term2 = in.readLong()
          val prev  = ser.read(in)
          Some(EntrySingle(term2, prev))
        case 2 =>
          // there is already a map found
          val m = handler.readIndexMap[A](in, tx)(index, ser)
          Some(EntryMap(m))
        case _ => None // this would be a partial hash which we don't use
      }
    })(tx) 
    
    eOpt match {
      // with the previous entry read, react as follows:
      // if there is a single entry, construct a new ancestor.map with the
      // entry's value taken as root value
      case Some(EntrySingle(prevTerm, prevValue)) if term != prevTerm =>  // skip to overwrite existing entries
        putFullMap[A](key, index, term, value, prevTerm, prevValue)(tx, ser)
      // if there is an existing map, simply add the new value to it
      case Some(EntryMap(m)) =>
        m.add(term, value)(tx)
      // if there is no previous entry...
      case _ =>
        // we may write a single entry if and only if the value can be seen
        // as a root value (the write path corresponds to the construction
        // of the entity, e.g. path == <term, term>; or the entity was
        // re-written in the tree root, hence path.suffix == <term, term>)
        val indexTerm = index.term
        if (term == indexTerm) {
          putPartials(key, index)(tx)
          putFullSingle[A](key, index, term, value)(tx, ser)
          // otherwise, we must read the root value for the entity, and then
          // construct a new map containing that root value along with the
          // new value
        } else {
          // however, there is a particular case of our unorthodox skip list
          // structure -- it allocates new variables while only maintaining one
          // main id. this yields perfectly valid structures which cannot be
          // accessed other than in the write path, although term and indexTerm
          // are _not_ the same. example: insertion in a full leaf -- a new
          // branch is created with two down vars which have the tree id's
          // index term and the write version's term. their initial value is
          // the newly created split leaves, there are no previous values.
          //
          // we may forbid this behaviour in future versions, but for now let's
          // be generous and allow it, by checking _if_ a previous value exists.
          // if not -- go again for the full single entry...
          //                  val prevValue = get[ A ]( key, path ).getOrElse(
          //                     sys.error( path.mkString( "Expected previous value not found for <" + key + " @ ", ",", ">" ))
          //                  )
          //                  putPartials( key, index )
          //                  putFullMap[ A ]( key, index, term, value, indexTerm, prevValue )
          getImmutable[A](key, tx) match {
            case Some(prevValue) =>
              putPartials(key, index)(tx)
              putFullMap   [A](key, index, term, value, indexTerm, prevValue)(tx, ser)

            case _ =>
              putPartials(key, index)(tx)
              putFullSingle[A](key, index, term, value)(tx, ser)
          }
        }
    }
  }

  final def put[A](key: K, value: A, tx: T)(implicit path: tx.Acc, ser: TSerializer[T, A]): Unit = {
    val (index, term) = path.splitIndex
    val arrOut  = DataOutput()
    ser.write(value, arrOut)
    val arr     = arrOut.toByteArray
    // first we need to see if anything has already been written to the index of the write path
    store.flatGet[Entry[T, Array[Byte]]]({ out =>
      writeKey(key, out)
      out.writeLong(index.sum)
    })({ in =>
      (in.readByte(): @switch) match {
        case 1 =>
          // a single 'root' value is found. extract it for successive re-write.
          val term2 = in.readLong()
          val arr   = ByteArraySerializer.read(in)
          // val prev = ser.read(in)
          Some(EntrySingle(term2, arr /* prev */))
        case 2 =>
          // there is already a map found
          val m = handler.readIndexMap[Array[Byte]/* A */](in, tx)(index, ByteArraySerializer)
          Some(EntryMap(m))
        case _ => None // this would be a partial hash which we don't use
      }
    })(tx) match {
      // with the previous entry read, react as follows:
      // if there is a single entry, construct a new ancestor.map with the
      // entry's value taken as root value
      case Some(EntrySingle(prevTerm, prevArr)) if term != prevTerm =>  // skip to overwrite existing entries
        putFullMap[Array[Byte]](key, index, term, arr, prevTerm, prevArr)(tx, ByteArraySerializer)
      // if there is an existing map, simply add the new value to it
      case Some(EntryMap(m)) =>
        m.add(term, arr)(tx)
      // if there is no previous entry...
      case _ =>
        // we may write a single entry if and only if the value can be seen
        // as a root value (the write path corresponds to the construction
        // of the entity, e.g. path == <term, term>; or the entity was
        // re-written in the tree root, hence path.suffix == <term, term>)
        val indexTerm = index.term
        if (term == indexTerm) {
          putPartials(key, index)(tx)
          putFullSingle[Array[Byte]](key, index, term, arr)(tx, ByteArraySerializer)
          // otherwise, we must read the root value for the entity, and then
          // construct a new map containing that root value along with the
          // new value
        } else {
          val prevAccI  = path.index
          val prevAcc   = prevAccI :+ prevAccI.last   // path where term is replaced by index-term (tree root)
          val prevOpt   = get[A](key, tx)(prevAcc, ser)
          // println(s"prev value for new full tree, input = $prevAcc; path = $path; prev = $prevOpt")
          prevOpt match {
            case Some(prevValue) =>
              // re-serialize previous value
              arrOut.reset()
              ser.write(prevValue, arrOut)
              val prevArr = arrOut.toByteArray
              putPartials(key, index)(tx)
              putFullMap   [Array[Byte]](key, index, term, arr, indexTerm, prevArr)(tx, ByteArraySerializer)

            case _ =>
              putPartials(key, index)(tx)
              putFullSingle[Array[Byte]](key, index, term, arr                    )(tx, ByteArraySerializer)
          }
        }
    }
  }

  private[this] def putFullMap[A](key: K, index: Access[T], term: Long, value: A, prevTerm: Long, prevValue: A)
                                 (implicit tx: T, ser: ConstantSerializer[A]): Unit = {
    //         require( prevTerm != term, "Duplicate flush within same transaction? " + term.toInt )
    //         require( prevTerm == index.term, "Expected initial assignment term " + index.term.toInt + ", but found " + prevTerm.toInt )
    // create new map with previous value
    val m = handler.newIndexMap[A](tx, prevTerm, prevValue)(index, ser)
    // store the full value at the full hash (path.sum)
    store.put { out =>
      writeKey(key, out) // out.writeInt( key )
      out.writeLong(index.sum)
    } { out =>
      out.writeByte(2) // aka map entry
      m.write(out)
    }
    // then add the new value
    m.add(term, value)
  }

  def remove(key: K, tx: T)(implicit index: tx.Acc): Boolean = {
    Hashing.foreachPrefix(index, hash => {
      store.contains({ out =>
        writeKey(key, out)
        out.writeLong(hash)
      })(tx)
    }) {
      case (hash, _) =>
        store.remove({ out =>
          writeKey(key, out)
          out.writeLong(hash)
        })(tx)
    }
    store.remove({ out =>
      writeKey(key, out)
      out.writeLong(index.sum)
    })(tx)
  }

  // stores the prefixes
  private[this] def putPartials(key: K, index: Access[T])(implicit tx: T): Unit =
    Hashing.foreachPrefix(index, hash => {
      val res = store.contains { out =>
        writeKey(key, out)
        out.writeLong(hash)
      }
      res
    }) {
      // for each key which is the partial sum, we store preSum which is the longest prefix of \tau' in \Pi
      case (hash, preSum) => store.put { out =>
        writeKey(key, out) // out.writeInt( key )
        out.writeLong(hash)
      } { out =>
        out.writeByte(0) // aka entry pre
        out.writeLong(preSum)
      }
    }

  // store the full value at the full hash (path.sum)
  private[this] def putFullSingle[A](key: K, index: Access[T], term: Long, value: A)
                                    (implicit tx: T, ser: ConstantSerializer[A]): Unit =
    store.put { out =>
      writeKey(key, out)
      out.writeLong(index.sum)
    } { out =>
      out.writeByte(1) // aka entry single
      out.writeLong(term)
      ser.write(value, out)
    }

  final def getImmutable[A](key: K, tx: T)(implicit path: tx.Acc, ser: ConstantSerializer[A]): Option[A] = {
    if (path.isEmpty) return None
    val (maxIndex, maxTerm) = path.splitIndex
    getWithPrefixLen[A](key, maxIndex, maxTerm)(tx, ser).map(_.value)
  }

  final def get[A](key: K, tx: T)(implicit path: tx.Acc, ser: TSerializer[T, A]): Option[A] = {
    if (path.isEmpty) return None
    val (maxIndex, maxTerm) = path.splitIndex
    val opt = getWithPrefixLen[Array[Byte]](key, maxIndex, maxTerm)(tx, ByteArraySerializer)
    opt.map { wp =>
      //            (path.dropAndReplaceHead( preLen, writeTerm ), value)
      val access  = wp.term +: path.drop(wp.len)
      val in      = DataInput(wp.value)
      tx.withReadAccess(access)(ser.readT(in)(tx))
    }
  }

  private[this] def getWithPrefixLen[A](key: K, maxIndex: Access[T], maxTerm: Long)
                                       (implicit tx: T, ser: ConstantSerializer[A]): Option[WithPrefix[A]] = {
    val preLen = Hashing.maxPrefixLength(maxIndex, hash => store.contains { out =>
      writeKey(key, out)
      out.writeLong(hash)
    })
    val (index, term) = if (preLen == maxIndex.size) {
      // maximum prefix lies in last tree
      (maxIndex, maxTerm)
    } else {
      // prefix lies in other tree
      maxIndex.splitAtIndex(preLen)
    }
    val preSum = index.sum
    store.flatGet { out =>
      writeKey(key, out)
      out.writeLong(preSum)
    } { in =>
      (in.readByte(): @switch) match {
        case 0 => // partial hash
          val hash = in.readLong()
          //                  EntryPre[ S ]( hash )
          val (fullIndex, fullTerm) = maxIndex.splitAtSum(hash)
          getWithPrefixLen[A](key, fullIndex, fullTerm)

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

          // --- ADDENDUM 2 (23-Oct-12): the above addendum fails for event variables which may well be
          // uninitialised. Therefore go back to the original thought...

          val term2 = in.readLong()
          val isOk  = if (isOblivious) {
            handler.isAncestor(/* index, */ term2, term)
          } else true

          if (isOk) {
            val value = ser.read(in)
            Some(new WithPrefix(preLen, term2, value))
          } else {
            None
          }

        case 2 =>
          val m = handler.readIndexMap[A](in, tx)(index, ser)
          if (isOblivious) {
            m.nearestOption(term).map {
              case (term2, value) => new WithPrefix(preLen, term2, value)
            }

          } else {
            val (term2, value) = m.nearest(term)
            Some(new WithPrefix(preLen, term2, value))
          }
      }
    }
  }
}

final class ConfluentIntMapImpl[T <: Txn[T]](protected val store: DataStore, protected val handler: IndexMapHandler[T],
                                             protected val isOblivious: Boolean)
  extends DurableConfluentMapImpl[T, Int] {

  protected def writeKey(key: Int, out: DataOutput): Unit = out.writeInt(key)
}

final class ConfluentLongMapImpl[T <: Txn[T]](protected val store: DataStore, protected val handler: IndexMapHandler[T],
                                              protected val isOblivious: Boolean)
  extends DurableConfluentMapImpl[T, Long] {

  protected def writeKey(key: Long, out: DataOutput): Unit = out.writeLong(key)
}