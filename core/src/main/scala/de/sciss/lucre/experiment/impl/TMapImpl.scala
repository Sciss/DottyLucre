/*
 *  TMapImpl.scala
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
package impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.experiment.TMap.Key
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.reflect.ClassTag

object TMapImpl {
  def apply[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]]()(implicit tx: T, 
                                                            keyType: Key[K]): TMap.Modifiable[T, K, Repr] = {
    val targets = Event.Targets[T]()
    new Impl[T, K, Repr](tx)(targets) { self =>
      val peer: SkipList.Map[T, K, List[Entry[K, V]]] = SkipList.Map.empty(tx, keyOrdering, self.keyType.serializer,
        TSerializer.list(entrySerializer))
    }
  }

  def serializer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]]: TSerializer[T, TMap[T, K, Repr]] =
    new Ser[T, K, Repr]

  def modSerializer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]]: TSerializer[T, TMap.Modifiable[T, K, Repr]] =
    new ModSer[T, K, Repr]

  private class Ser[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]] // (implicit keyType: Key[K])
    extends ObjSerializer[T, TMap[T, K, Repr]] {

    def tpe: Obj.Type = TMap
  }

  private class ModSer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]] // (implicit keyType: Key[K])
    extends ObjSerializer[T, TMap.Modifiable[T, K, Repr]] {

    def tpe: Obj.Type = TMap
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] = {
    val targets   = Event.Targets.read(in, tx)
    val keyTypeId = in.readInt()
    val keyType   = Key(keyTypeId) // Obj.getType(keyTypeId).asInstanceOf[Key[_]]
    mkRead(in, tx, targets)(acc, keyType)
  }

  private def mkRead[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T, targets: Event.Targets[T])
                                                                  (implicit acc: tx.Acc, keyType: Key[K]): Impl[T, K, Repr] =
    new Impl[T, K, Repr](tx)(targets) { self =>
      val peer: SkipList.Map[T, K, List[Entry[K, V]]] =
        SkipList.Map.read[T, K, List[Entry[K, V]]](in, tx)(acc, keyOrdering, self.keyType.serializer,
          TSerializer.list(entrySerializer))
    }

  private final class Entry[K, V](val key: K, val value: V)

  private abstract class Impl[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]](tx: T)
                                                                           (protected val targets: Event.Targets[T])
                                                                           (implicit val keyType: Key[K])
    extends TMap.Modifiable[T, K, Repr] with SingleEventNode[T, TMap.Update[T, K, Repr]] {
    map =>

    final def tpe: Obj.Type = TMap

    // ---- abstract ----

    protected def peer: SkipList.Map[T, K, List[Entry[K, V]]]

    // ---- implemented ----

    private[experiment] def copy[Out <: Txn[Out]]()(implicit txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      val res = TMap.Modifiable[Out, K, Elem /* Repr */]()
      iterator.foreach { case (k, v) =>
        res.put(k, context(v))
      }
      res
    }

    implicit object keyOrdering extends scala.Ordering[K] /*Ordering[T, K]*/ {
      def compare(a: K, b: K) /*(implicit tx: T)*/: Int = {
        val ah = a.hashCode() // ##
        val bh = b.hashCode() // ##
        if (ah < bh) -1 else if (ah > bh) 1 else 0
      }
    }

    object entrySerializer extends TSerializer[T, Entry[K, V]] {
      def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Entry[K, V] = {
        val key   = keyType.serializer.read(in)
        val value = Elem.read(in, tx).asInstanceOf[V]
        new Entry[K, V](key, value)
      }

      def write(entry: Entry[K, V], out: DataOutput): Unit = {
        keyType.serializer.write(entry.key, out)
        entry.value.write(out)
      }
    }

    final def contains(key: K): Boolean    = peer.get(key).exists(vec => vec.exists(_.key === key))
    final def get     (key: K): Option[V]  = peer.get(key).flatMap { vec =>
      vec.collectFirst {
        case entry if entry.key === key => entry.value
      }
    }

    final def iterator: Iterator[(K, V)] = peer.iterator.flatMap {
      case (key, vec) => vec.map(entry => key -> entry.value)
    }
    final def keysIterator  : Iterator[K] = peer.valuesIterator.flatMap(_.map(_.key  ))
    final def valuesIterator: Iterator[V] = peer.valuesIterator.flatMap(_.map(_.value))

    final def $[R[~ <: Txn[~]] <: Repr[~]](key: K)(implicit ct: ClassTag[R[T]]): Option[R[T]] =
      peer.get(key).flatMap { vec =>
        vec.collectFirst {
          case entry if entry.key === key && ct.runtimeClass.isAssignableFrom(entry.value.getClass) =>
            entry.value.asInstanceOf[R[T]]
        }
      }

    final def size: Int = {
      // XXX TODO: a bit ugly...
      var res = 0
      peer.valuesIterator.foreach(res += _.size)
      res
    }
    final def nonEmpty: Boolean  = peer.nonEmpty
    final def isEmpty : Boolean  = peer.isEmpty

    final def modifiableOption: Option[TMap.Modifiable[T, K, Repr]] = Some(this)

    protected final def writeData(out: DataOutput): Unit = {
      out.writeInt(keyType.typeId)
      peer.write(out)
    }

    protected final def disposeData(): Unit = peer.dispose()

    override def toString = s"Map$id"

    protected final def foreach(fun: Entry[K, V] => Unit): Unit =
      peer.valuesIterator.foreach(_.foreach(fun))

    object changed extends Changed
      with RootGeneratorEvent[T, TMap.Update[T, K, Repr]]

    private def fireAdded(key: K, value: V): Unit =
      changed.fire(TMap.Update[T, K, Repr](map, TMap.Added[/*T,*/ K, V](key, value) :: Nil))(tx)

    private def fireRemoved(key: K, value: V): Unit =
      changed.fire(TMap.Update[T, K, Repr](map, TMap.Removed[/*T,*/ K, V](key, value) :: Nil))(tx)

    private def fireReplaced(key: K, before: V, now: V): Unit =
      changed.fire(TMap.Update[T, K, Repr](map, TMap.Replaced[/*T,*/ K, V](key, before = before, now = now) :: Nil))(tx)

    final def +=(kv: (K, V)): this.type = {
      put(kv._1, kv._2)
      this
    }

    final def -=(key: K): this.type = {
      remove(key)
      this
    }

    final def put(key: K, value: V): Option[V] = {
      val entry     = new Entry[K, V](key, value)
      val oldVec    = peer.get(key).getOrElse(Nil)
      val idx       = oldVec.indexWhere(_.key === key)
      val found     = idx >= 0
      val newVec    = if (found) oldVec.updated(idx, entry) else oldVec :+ entry
      peer.put(key, newVec)

      if (found) {
        val oldEntry = oldVec(idx)
        fireReplaced(key, before = oldEntry.value, now = value)
      } else {
        fireAdded(key, value)
      }

      if (found) Some(oldVec(idx).value) else None
    }

    final def remove(key: K): Option[V] = {
      val oldVec  = peer.get(key).getOrElse(Nil)
      val idx     = oldVec.indexWhere(_.key === key)
      if (idx < 0) return None

      val entry   = oldVec(idx)
      val value   = entry.value
      val newVec  = oldVec.patch(idx, Nil, 1)
      if (newVec.isEmpty) {
        peer.remove(key)
      } else {
        peer.put(key, newVec)
      }

      // unregisterElement(entry)
      fireRemoved(key, value)

      Some(value)
    }
  }
}