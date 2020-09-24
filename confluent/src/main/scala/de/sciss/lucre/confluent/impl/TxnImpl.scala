/*
 *  TxnImpl.scala
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

import de.sciss.lucre.confluent.Log.log
import de.sciss.lucre.confluent.impl.{PathImpl => Path}
import de.sciss.lucre.impl.BasicTxnImpl
import de.sciss.lucre.{Confluent, Durable, DurableLike, IdentMap, InMemory, Obj, ReactionMap, MapObj}
import de.sciss.serial.{ConstFormat, DataInput, TFormat}

import scala.collection.immutable.{IndexedSeq => Vec, Queue => IQueue}
import scala.concurrent.stm.{InTxn, Txn => ScalaTxn}

trait TxnMixin[Tx <: Txn[Tx]]
  extends Txn[Tx] with BasicTxnImpl[Tx] with VersionInfo.Modifiable {
  self: Tx =>

  type T = Tx

  // ---- abstract ----

  protected def flushCaches(meld: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]]): Unit

  // ---- info ----

  final private[lucre] def reactionMap: ReactionMap[T] = system.reactionMap

  final def info: VersionInfo.Modifiable = this

  final var message: String = ""
  final val timeStamp: Long = System.currentTimeMillis()

  // ---- init ----

  /*
   * Because durable maps are persisted, they may be deserialized multiple times per transaction.
   * This could potentially cause a problem: imagine two instances A1 and A2. A1 is read, a `put`
   * is performed, making A1 call `markDirty`. Next, A2 is read, again a `put` performed, and A2
   * calls `markDirty`. Next, another `put` on A1 is performed. In the final flush, because A2
   * was marked after A1, it's cached value will override A2's, even though it is older.
   *
   * To avoid that, durable maps are maintained by their id's in a transaction local map. That way,
   * only one instance per id is available in a single transaction.
   */
  // private var durableIdMaps     = IntMap.empty[DurableIdMapImpl[_, _]]
  private var meld              = MeldInfo.empty[T]
  private var dirtyMaps         = Vector.empty[Cache[T]]
  private var beforeCommitFuns  = IQueue.empty[T => Unit]

  // indicates whether we have added cache maps to dirty maps
  private var markDirtyFlag       = false
  // indicates whether we have added cache maps that require version updates (e.g. non-event writes)
  private var markNewVersionFlag  = false
  // indicates whether any before commit handling is needed
  // (either dirtyMaps got non-empty, or a user before-commit handler got registered)
  private var markBeforeCommitFlag = false

  final protected def meldInfo: MeldInfo[T] = meld

  final private def markDirty(): Unit =
    if (!markDirtyFlag) {
      markDirtyFlag = true
      addDirtyCache(fullCache)
      // addDirtyCache(partialCache)
    }

  final def addDirtyCache(map: Cache[T]): Unit = {
    dirtyMaps :+= map
    markNewVersionFlag = true
    markBeforeCommit()
  }

  /** A local cache is one which is re-created upon application restart. It should
    * probably be called transient instead of local, but we already have `stm.LocalVar`...
    *
    * If the dirty maps only contain local caches, no new version is created upon flush.
    */
  final def addDirtyLocalCache(map: Cache[T]): Unit = {
    dirtyMaps :+= map
    markBeforeCommit()
  }

  final override def beforeCommit(fun: T => Unit): Unit = {
    beforeCommitFuns = beforeCommitFuns.enqueue(fun)
    markBeforeCommit()
  }

  private def markBeforeCommit(): Unit =
    if (!markBeforeCommitFlag) {
      markBeforeCommitFlag = true
      log("....... txn dirty .......")
      ScalaTxn.beforeCommit(handleBeforeCommit)(peer)
    }

  // first execute before commit handlers, then flush
  private def handleBeforeCommit(itx: InTxn): Unit = {
    while (beforeCommitFuns.nonEmpty) {
      val (fun, q) = beforeCommitFuns.dequeue
      beforeCommitFuns = q
      fun(this)
    }
    flushCaches(meld, markNewVersionFlag, dirtyMaps)
  }

  final protected def fullCache: CacheMap.Durable[T, Int, DurablePersistentMap[T, Int]] = system.fullCache
  // final protected def partialCache  = system.partialCache

  final def newId(): Id = {
    val res = new ConfluentId[T](system.newIdValue()(this), Path.empty[T])
    log(s"txn newId $res")
    res
  }

  // ---- access ----

  private[this] var _readAccess: Acc = _  // could be initialized as `inputAccess`, but that's a `val`

  private[confluent] override final def withReadAccess[A](path: Access[Tx])(body: => A): A = {
    val oldPath = _readAccess
    _readAccess = path
    try {
      body
    } finally {
      _readAccess = oldPath
    }
  }

  private[confluent] def readAccess = _readAccess

  // ---- context ----

  // def newContext(): S#Context = ...

  // ---- attributes ----

  def attrMap(obj: Obj[T]): Obj.AttrMap[T] = {
    val id        = obj.id.!(this)
    val mBase     = id.base | 0x80000000  // XXX TODO --- a bit cheesy to throw away one bit entirely
    val mapOpt    = fullCache.getCacheTxn[Obj.AttrMap[T]](mBase, this)(id.path, Obj.attrMapFormat)
    mapOpt.getOrElse {
      val map = MapObj.Modifiable[T, String, Obj]()(MapObj.Key.String, this)
      fullCache.putCacheTxn[Obj.AttrMap[T]](mBase, map, this)(id.path, Obj.attrMapFormat)
      markDirty()
      map
    }
  }

  override def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]] = {
    val id        = obj.id.!(this)
    val mBase     = id.base | 0x80000000  // XXX TODO --- a bit cheesy to throw away one bit entirely
    fullCache.getCacheTxn[Obj.AttrMap[T]](mBase, this)(id.path, Obj.attrMapFormat)
  }

  // ----

  final def readTreeVertexLevel(term: Long): Int = {
    system.store.get(out => {
      out.writeByte(0)
      out.writeInt(term.toInt)
    })(in => {
      in./* PACKED */ readInt()  // tree index!
      in./* PACKED */ readInt()
    })(this).getOrElse(sys.error(s"Trying to access non-existent vertex ${term.toInt}"))
  }

  final def addInputVersion(path: Access[T]): Unit = {
    val sem1 = path.seminal
    val sem2 = inputAccess.seminal
    if (sem1 == sem2) return
    if (sem1 != sem2) {
      val m = meld
      // note: before we were reading the index tree; but since only the level
      // is needed, we can read the vertex instead which also stores the
      // the level.
      //               val tree1 = readIndexTree(sem1.head)
      val tree1Level = readTreeVertexLevel(sem1.head)
      val m1 = if (m.isEmpty) {
        val tree2Level = readTreeVertexLevel(sem2.head)
        m.add(tree2Level, sem2)
      } else m
      meld = m1.add(tree1Level, sem1)
    }
  }

  final def newHandle[A](value: A)(implicit format: TFormat[T, A]): Source[T, A] = {
    val h = new HandleImpl[T, A](value, inputAccess.index)
    addDirtyLocalCache(h)
    h
  }

  final def newHandleM[A](value: A)(implicit format: TFormat[T, A]): Source[T, A] =
    newHandle(value)

  final def getNonTxn[A](id: Id)(implicit format: ConstFormat[A]): A = {
    log(s"txn get $id")
    fullCache.getCacheNonTxn[A](id.base, this)(id.path, format).getOrElse(sys.error(s"No value for $id"))
  }

  final def getTxn[A](id: Id)(implicit format: TFormat[T, A]): A = {
    log(s"txn get' $id")
    fullCache.getCacheTxn[A](id.base, this)(id.path, format).getOrElse(sys.error(s"No value for $id"))
  }

  final def putTxn[A](id: Id, value: A)(implicit format: TFormat[T, A]): Unit = {
    fullCache.putCacheTxn[A](id.base, value, this)(id.path, format)
    markDirty()
  }

  final def putNonTxn[A](id: Id, value: A)(implicit format: ConstFormat[A]): Unit = {
    fullCache.putCacheNonTxn[A](id.base, value, this)(id.path, format)
    markDirty()
  }

//  final def putPartial[A](id: S#Id, value: A)(implicit format: serial.Format[T, S#Acc, A]): Unit = {
//    partialCache.putPartial(id.base, id.path, value)(this, ser)
//    markDirty()
//  }
//
//  final def getPartial[A](id: S#Id)(implicit format: serial.Format[T, S#Acc, A]): A =
//    partialCache.getPartial[A](id.base, id.path)(this, ser).getOrElse(sys.error(s"No value for $id"))

  final def removeFromCache(id: Id): Unit =
    fullCache.removeCacheOnly(id.base, this)(id.path)

//  final def newVar[A](pid: Id, init: A)(implicit format: TFormat[T, A]): Var[A] = {
//    val res = makeVar[A](alloc(pid))
//    log(s"txn newVar $res")
//    res.setInit(init)
//    res
//  }
//
//  final def newBooleanVar(pid: Id, init: Boolean): Var[Boolean] = {
//    val id  = alloc(pid)
//    val res = new BooleanVar(this, id)
//    log(s"txn newVar $res")
//    res.setInit(init)
//    res
//  }
//
//  final def newIntVar(pid: Id, init: Int): Var[Int] = {
//    val id  = alloc(pid)
//    val res = new IntVar(this, id)
//    log(s"txn newVar $res")
//    res.setInit(init)
//    res
//  }
//
//  final def newLongVar(pid: Id, init: Long): Var[Long] = {
//    val id  = alloc(pid)
//    val res = new LongVar(this, id)
//    log(s"txn newVar $res")
//    res.setInit(init)
//    res
//  }

  override final def newIdentMap[A]: IdentMap[T, A] = {
    val map = InMemoryConfluentMap.newIntMap[T]
    new InMemoryIdMapImpl[T, A](map)
  }

  override final def readId(in: DataInput): Id = {
    val base  = in./* PACKED */ readInt()
    val res   = new ConfluentId(base, Path.readAndAppend[T](in, readAccess)(this))
    log(s"txn readId $res")
    res
  }

  override def toString = s"confluent.Sys#Tx$inputAccess"
}

trait RegularTxnMixin[Tx <: Txn[Tx], D <: DurableLike.Txn[D]] extends TxnMixin[Tx] {
  self: Tx =>

  protected def cursorCache: Cache[T]

  final protected def flushCaches(meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]]): Unit =
    system.flushRegular(meldInfo, newVersion, caches :+ cursorCache)(this)

  override def toString = s"Confluent#Tx$inputAccess"
}

trait RootTxnMixin[Tx <: Txn[Tx], D <: DurableLike.Txn[D]]
  extends TxnMixin[Tx] {
  self: Tx =>

  final val inputAccess: Access[T] = Path.root[T]

  final def isRetroactive = false

  final protected def flushCaches(meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]]): Unit =
    system.flushRoot(meldInfo, newVersion, caches)(this)

  override def toString = "Confluent.RootTxn"
}

private[impl] sealed trait TxnImpl extends Confluent.Txn /*Txn[Confluent.Txn]*/ {
  final lazy val inMemory: InMemory.Txn = system.inMemory.wrap(peer)

  def inMemoryBridge: (Confluent.Txn => InMemory.Txn) = _.inMemory
}

private[impl] final class RegularTxn(val system: Confluent, val durable: Durable.Txn,
                               val inputAccess: Access[Confluent.Txn], val isRetroactive: Boolean,
                               val cursorCache: Cache[Confluent.Txn])
  extends RegularTxnMixin[Confluent.Txn, Durable.Txn] with TxnImpl {

  lazy val peer: InTxn = durable.peer
}

private[impl] final class RootTxn(val system: Confluent, val peer: InTxn)
  extends RootTxnMixin[Confluent.Txn, Durable.Txn] with TxnImpl {

  lazy val durable: Durable.Txn = {
    log("txn durable")
    system.durable.wrap(peer)
  }
}