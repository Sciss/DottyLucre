///*
// *  MapImpl.scala
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
//package de.sciss.lucre.event.impl
//
//import de.sciss.equal.Implicits._
//import de.sciss.lucre.event.Map.{Key, Modifiable}
//import de.sciss.lucre.stm.impl.ObjSerializer
//import de.sciss.lucre.stm.{Copy, Elem, Obj, TxSerializer, Txn}
//import de.sciss.lucre.{event => evt}
//import de.sciss.serial.{DataInput, DataOutput, Serializer}
//
//import scala.reflect.ClassTag
//
//object MapImpl {
//  def apply[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]](implicit tx: T, keyType: Key[K]): Modifiable[T, K, Repr] = {
//    val targets = evt.Targets[T]
//    new Impl[T, K, Repr](targets) { self =>
//      val peer: SkipList.Map[T, K, List[Entry[K, V]]] = SkipList.Map.empty(tx, keyOrdering, self.keyType.serializer,
//        TxSerializer.list(entrySerializer))
//    }
//  }
//
//  def serializer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]]: TxSerializer[T, evt.Map[T, K, Repr]] =
//    new Ser[T, K, Repr]
//
//  def modSerializer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]]: TxSerializer[T, Modifiable[T, K, Repr]] =
//    new ModSer[T, K, Repr]
//
//  private class Ser[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]] // (implicit keyType: Key[K])
//    extends ObjSerializer[T, evt.Map[T, K, Repr]] {
//
//    def tpe: Obj.Type = evt.Map
//  }
//
//  private class ModSer[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]] // (implicit keyType: Key[K])
//    extends ObjSerializer[T, Modifiable[T, K, Repr]] {
//
//    def tpe: Obj.Type = evt.Map
//  }
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Obj[T] = {
//    val targets   = evt.Targets.read(in, tx)
//    val keyTypeId = in.readInt()
//    val keyType   = Key(keyTypeId) // Obj.getType(keyTypeId).asInstanceOf[Key[_]]
//    mkRead(in, tx, targets)(acc, keyType)
//  }
//
//  private def mkRead[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]](in: DataInput, tx: T, targets: evt.Targets[T])
//                                               (implicit acc: tx.Acc, keyType: Key[K]): Impl[T, K, Repr] =
//    new Impl[T, K, Repr](targets) { self =>
//      val peer: SkipList.Map[T, K, List[Entry[K, V]]] =
//        SkipList.Map.read[T, K, List[Entry[K, V]]](in, tx)(acc, keyOrdering, self.keyType.serializer,
//          TxSerializer.list(entrySerializer))
//    }
//
//  private final class Entry[K, V](val key: K, val value: V)
// 
//  private abstract class Impl[T <: Txn[T], K, Repr[~ <: Txn[~]] <: Elem[~]](protected val targets: evt.Targets[T])
//                                                          (implicit val keyType: Key[K])
//    extends Modifiable[T, K, Repr] with evt.impl.SingleNode[T, evt.Map.Update[T, K, Repr]] {
//    map =>
//
//    final def tpe: Obj.Type = evt.Map
//
//    // ---- abstract ----
//
//    protected def peer: SkipList.Map[T, K, List[Entry[K, V]]]
//    
//    // ---- implemented ----
//
//    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
//      val res = evt.Map.Modifiable[Out, K, Elem /* Repr */]
//      iterator.foreach { case (k, v) =>
//        res.put(k, context(v))
//      }
//      res
//    }
//
//    implicit object keyOrdering extends scala.Ordering[K] /*Ordering[T, K]*/ {
//      def compare(a: K, b: K) /*(implicit tx: T)*/: Int = {
//        val ah = a.hashCode() // ##
//        val bh = b.hashCode() // ##
//        if (ah < bh) -1 else if (ah > bh) 1 else 0
//      }
//    }
//
//    object entrySerializer extends TxSerializer[T, Entry[K, V]] {
//      def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Entry[K, V] = {
//        val key   = keyType.serializer.read(in)
//        val value = Elem.read(in, tx).asInstanceOf[V]
//        new Entry[K, V](key, value)
//      }
//
//      def write(entry: Entry[K, V], out: DataOutput): Unit = {
//        keyType.serializer.write(entry.key, out)
//        entry.value.write(out)
//      }
//    }
//
//    final def contains(key: K)(implicit tx: T): Boolean    = peer.get(key).exists(vec => vec.exists(_.key === key))
//    final def get     (key: K)(implicit tx: T): Option[V]  = peer.get(key).flatMap { vec =>
//      vec.collectFirst {
//        case entry if entry.key === key => entry.value
//      }
//    }
//
//    final def iterator(implicit tx: T): Iterator[(K, V)] = peer.iterator.flatMap {
//      case (key, vec) => vec.map(entry => key -> entry.value)
//    }
//    final def keysIterator  (implicit tx: T): Iterator[K] = peer.valuesIterator.flatMap(_.map(_.key  ))
//    final def valuesIterator(implicit tx: T): Iterator[V] = peer.valuesIterator.flatMap(_.map(_.value))
//
//    final def $[R[~ <: Txn[~]] <: Repr[~]](key: K)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]] =
//      peer.get(key).flatMap { vec =>
//        vec.collectFirst {
//          case entry if entry.key === key && ct.runtimeClass.isAssignableFrom(entry.value.getClass) =>
//            entry.value.asInstanceOf[R[T]]
//        }
//      }
//
//    final def size(implicit tx: T): Int = {
//      // XXX TODO: a bit ugly...
//      var res = 0
//      peer.valuesIterator.foreach(res += _.size)
//      res
//    }
//    final def nonEmpty(implicit tx: T): Boolean  = peer.nonEmpty
//    final def isEmpty (implicit tx: T): Boolean  = peer.isEmpty
//
//    final def modifiableOption: Option[Modifiable[T, K, Repr]] = Some(this)
//
//    protected final def writeData(out: DataOutput): Unit = {
//      out.writeInt(keyType.typeId)
//      peer.write(out)
//    }
//
//    protected final def disposeData()(implicit tx: T): Unit = peer.dispose()
//
//    override def toString = s"Map$id"
//    
//    protected final def foreach(fun: Entry[K, V] => Unit)(implicit tx: T): Unit =
//      peer.valuesIterator.foreach(_.foreach(fun))
//
//    object changed extends Changed
//      with evt.impl.GeneratorEvent[T, evt.Map.Update[T, K, Repr]] with evt.impl.Root[T, evt.Map.Update[T, K, Repr]]
//
//    private[this] def fireAdded(key: K, value: V)(implicit tx: T): Unit =
//      changed.fire(evt.Map.Update[T, K, Repr](map, evt.Map.Added[T, K, V](key, value) :: Nil))
//
//    private[this] def fireRemoved(key: K, value: V)(implicit tx: T): Unit =
//      changed.fire(evt.Map.Update[T, K, Repr](map, evt.Map.Removed[T, K, V](key, value) :: Nil))
//
//    private[this] def fireReplaced(key: K, before: V, now: V)(implicit tx: T): Unit =
//      changed.fire(evt.Map.Update[T, K, Repr](map, evt.Map.Replaced[T, K, V](key, before = before, now = now) :: Nil))
//
//    final def +=(kv: (K, V))(implicit tx: T): this.type = {
//      put(kv._1, kv._2)
//      this
//    }
//
//    final def -=(key: K)(implicit tx: T): this.type = {
//      remove(key)
//      this
//    }
//
//    final def put(key: K, value: V)(implicit tx: T): Option[V] = {
//      val entry     = new Entry[K, V](key, value)
//      val oldVec    = peer.get(key).getOrElse(Nil)
//      val idx       = oldVec.indexWhere(_.key === key)
//      val found     = idx >= 0
//      val newVec    = if (found) oldVec.updated(idx, entry) else oldVec :+ entry
//      peer.put(key, newVec)
//
//      if (found) {
//        val oldEntry = oldVec(idx)
//        fireReplaced(key, before = oldEntry.value, now = value)
//      } else {
//        fireAdded(key, value)
//      }
//
//      if (found) Some(oldVec(idx).value) else None
//    }
//
//    final def remove(key: K)(implicit tx: T): Option[V] = {
//      val oldVec  = peer.get(key).getOrElse(Nil)
//      val idx     = oldVec.indexWhere(_.key === key)
//      if (idx < 0) return None
//
//      val entry   = oldVec(idx)
//      val value   = entry.value
//      val newVec  = oldVec.patch(idx, Nil, 1)
//      if (newVec.isEmpty) {
//        peer.remove(key)
//      } else {
//        peer.put(key, newVec)
//      }
//
//      // unregisterElement(entry)
//      fireRemoved(key, value)
//
//      Some(value)
//    }
//  }
//}