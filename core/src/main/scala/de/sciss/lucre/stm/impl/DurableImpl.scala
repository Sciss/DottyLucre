///*
// *  DurableImpl.scala
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
//package de.sciss.lucre.stm.impl
//
//import de.sciss.equal.Implicits._
//import de.sciss.lucre.event.impl.ReactionMapImpl
//import de.sciss.lucre.event.{Observer, ReactionMap}
//import de.sciss.lucre.stm.{DataStore, Durable, DurableLike, IdentifierMap, InMemory, Obj, Source, Sys, Txn, TxnLike, Var, log}
//import de.sciss.lucre.{event => evt}
//import de.sciss.serial.{DataInput, DataOutput, Serializer}
//
//import scala.annotation.elidable
//import scala.concurrent.stm.{InTxn, Ref}
//
//object DurableImpl {
//  private type D[S <: DurableLike[S]] = DurableLike[S]
//
//  def apply(factory: DataStore.Factory, mainName: String): Durable = {
//    val mainStore   = factory.open(mainName)
//    apply(mainStore = mainStore)
//  }
//
//  def apply(mainStore: DataStore): Durable = new System(store = mainStore)
//
//  trait Mixin[S <: D[S], I <: Sys[I]] extends DurableLike[S] with ReactionMapImpl.Mixin[S] {
//    system =>
//
//    def store: DataStore
//
//    protected final val eventMap: IdentifierMap[S#Id, S#Tx, Map[Int, scala.List[Observer[S, _]]]] =
//      IdentifierMapImpl.newInMemoryIntMap[S#Id, S#Tx, Map[Int, scala.List[Observer[S, _]]]](_.id)
//
//    private[this] val idCntVar = step { implicit tx =>
//      val _id = store.get(_.writeInt(0))(_.readInt()).getOrElse(1)
//      val _idCnt = Ref(_id)
//      new CachedIntVar[S](0, _idCnt)
//    }
//
//    def root[A](init: S#Tx => A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      step { implicit tx =>
//        rootBody(init)
//      }
//
//    def rootJoin[A](init: S#Tx => A)(implicit tx: TxnLike, serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      rootBody(init)(wrap(tx.peer), serializer)
//
//    private[this] def rootBody[A](init: S#Tx => A)
//                                 (implicit tx: S#Tx, serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] = {
//      val rootId = 2 // 1 == reaction map!!!
//      if (exists(rootId)) {
//        new VarImpl[S, A](rootId, serializer)
//      } else {
//        val id = newIdValue()
//        require(id === rootId,
//          s"Root can only be initialized on an empty database (expected id count is $rootId but found $id)")
//        val res = new VarImpl[S, A](id, serializer)
//        res.setInit(init(tx))
//        res
//      }
//    }
//
//    // ---- cursor ----
//
//    def step[A](fun: S#Tx => A): A = stepTag(0L)(fun)
//
//    def stepTag[A](systemTimeNanos: Long)(fun: S#Tx => A): A = Txn.atomic { implicit itx =>
//      fun(wrap(itx, systemTimeNanos))
//    }
//
//    def position(implicit tx: S#Tx): S#Acc = ()
//
//    def debugListUserRecords()(implicit tx: S#Tx): Seq[Id] = {
//      val b   = Seq.newBuilder[Id]
//      val cnt = idCntVar()
//      var i   = 1
//      while (i <= cnt) {
//        if (exists(i)) b += new IdImpl[S](i)
//        i += 1
//      }
//      b.result()
//    }
//
//    def close(): Unit = store.close()
//
//    def numRecords(implicit tx: S#Tx): Int = store.numEntries
//
//    def numUserRecords(implicit tx: S#Tx): Int = math.max(0, numRecords - 1)
//
//    // this increases a durable variable, thus ensures markDirty() already
//    def newIdValue()(implicit tx: S#Tx): Int = {
//      val id = idCntVar() + 1
//      log(s"new   <$id>")
//      idCntVar() = id
//      id
//    }
//
//    def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: S#Tx): Unit = {
//      log(s"writeL <$id>")
//      store.put(_.writeLong(id))(valueFun)
//    }
//
//    def write(id: Int)(valueFun: DataOutput => Unit)(implicit tx: S#Tx): Unit = {
//      log(s"write <$id>")
//      store.put(_.writeInt(id))(valueFun)
//    }
//
//    def remove(id: Long)(implicit tx: S#Tx): Unit = {
//      log(s"removL <$id>")
//      store.remove(_.writeLong(id))
//    }
//
//    def remove(id: Int)(implicit tx: S#Tx): Unit = {
//      log(s"remov <$id>")
//      store.remove(_.writeInt(id))
//      //         tx.markDirty()
//    }
//
//    def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: S#Tx): Option[A] = {
//      log(s"readL  <$id>")
//      store.get(_.writeLong(id))(valueFun)
//    }
//
//    def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: S#Tx): A = {
//      log(s"read  <$id>")
//      store.get(_.writeInt(id))(valueFun).getOrElse(sys.error(s"Key not found $id"))
//    }
//
//    def exists(id: Int)(implicit tx: S#Tx): Boolean = store.contains(_.writeInt(id))
//
//    def exists(id: Long)(implicit tx: S#Tx): Boolean = store.contains(_.writeLong(id))
//  }
//
//  trait TxnMixin[S <: D[S]] extends DurableLike.Txn[S] with BasicTxnImpl[S] {
//    _: S#Tx =>
//
//    private[lucre] final def reactionMap: ReactionMap[S] = system.reactionMap
//
//    final def newId(): S#Id = new IdImpl[S](system.newIdValue()(this))
//
//    final def newVar[A](id: S#Id, init: A)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//      val res = new VarImpl[S, A](system.newIdValue()(this), ser)
//      res.setInit(init)(this)
//      res
//    }
//
//    final def newCachedVar[A](init: A)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//      val res = new CachedVarImpl[S, A](system.newIdValue()(this), Ref(init), ser)
//      res.writeInit()(this)
//      res
//    }
//
//    final def newBooleanVar(id: S#Id, init: Boolean): S#Var[Boolean] = {
//      val res = new BooleanVar[S](system.newIdValue()(this))
//      res.setInit(init)(this)
//      res
//    }
//
//    final def newIntVar(id: S#Id, init: Int): S#Var[Int] = {
//      val res = new IntVar[S](system.newIdValue()(this))
//      res.setInit(init)(this)
//      res
//    }
//
//    final def newCachedIntVar(init: Int): S#Var[Int] = {
//      val res = new CachedIntVar[S](system.newIdValue()(this), Ref(init))
//      res.writeInit()(this)
//      res
//    }
//
//    final def newLongVar(id: S#Id, init: Long): S#Var[Long] = {
//      val res = new LongVar[S](system.newIdValue()(this))
//      res.setInit(init)(this)
//      res
//    }
//
//    final def newCachedLongVar(init: Long): S#Var[Long] = {
//      val res = new CachedLongVar[S](system.newIdValue()(this), Ref(init))
//      res.writeInit()(this)
//      res
//    }
//
//    final def newVarArray[A](size: Int): Array[S#Var[A]] = new Array[Var[S#Tx, A]](size)
//
//    final def newInMemoryIdMap[A]: IdentifierMap[S#Id, S#Tx, A] =
//      IdentifierMapImpl.newInMemoryIntMap[S#Id, S#Tx, A](_.id)
//
//    final def readVar[A](pid: S#Id, in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//      val id = in./* PACKED */ readInt()
//      new VarImpl[S, A](id, ser)
//    }
//
//    final def readCachedVar[A](in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedVarImpl[S, A](id, Ref.make[A](), ser)
//      res.readInit()(this)
//      res
//    }
//
//    final def readBooleanVar(pid: S#Id, in: DataInput): S#Var[Boolean] = {
//      val id = in./* PACKED */ readInt()
//      new BooleanVar[S](id)
//    }
//
//    final def readIntVar(pid: S#Id, in: DataInput): S#Var[Int] = {
//      val id = in./* PACKED */ readInt()
//      new IntVar[S](id)
//    }
//
//    final def readCachedIntVar(in: DataInput): S#Var[Int] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedIntVar[S](id, Ref(0))
//      res.readInit()(this)
//      res
//    }
//
//    final def readLongVar(pid: S#Id, in: DataInput): S#Var[Long] = {
//      val id = in./* PACKED */ readInt()
//      new LongVar[S](id)
//    }
//
//    final def readCachedLongVar(in: DataInput): S#Var[Long] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedLongVar[S](id, Ref(0L))
//      res.readInit()(this)
//      res
//    }
//
//    final def readId(in: DataInput, acc: S#Acc): S#Id = {
//      val base = in./* PACKED */ readInt()
//      new IdImpl[S](base)
//    }
//
////    final def readPartialId(in: DataInput, acc: S#Acc): S#Id = readId(in, acc)
//
////    final def readDurableIdMap[A](in: DataInput)(implicit serializer: Serializer[S#Tx, S#Acc, A]): IdentifierMap[S#Id, S#Tx, A] = {
////      val base  = in./* PACKED */ readInt()
////      val mapId = new IdImpl[S](base)
////      new IdMapImpl[S, A](mapId)
////    }
//
//    final def newHandle[A](value: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      new EphemeralHandle(value)
//
//    // ---- context ----
//
//    // def newContext(): S#Context = ...
//
//    // ---- attributes ----
//
////    private[this] type AttrMap = SkipList.Map[S, String, Obj[S]]
//
//    def attrMap(obj: Obj[S]): Obj.AttrMap[S] = {
//      val mId = obj.id.id.toLong << 32
//      implicit val tx: S#Tx = this
//      val mapOpt: Option[Obj.AttrMap[S]] = system.tryRead(mId)(evt.Map.Modifiable.read[S, String, Obj](_, ()))
//      mapOpt.getOrElse {
//        val map = evt.Map.Modifiable[S, String, Obj]
//        system.write(mId)(map.write)
//        map
//      }
//    }
//
//    override def attrMapOption(obj: Obj[S]): Option[Obj.AttrMap[S]] = {
//      val mId = obj.id.id.toLong << 32
//      implicit val tx: S#Tx = this
//      system.tryRead(mId)(evt.Map.Modifiable.read[S, String, Obj](_, ()))
//    }
//
//    //    def attrGet(obj: Obj[S], key: String): Option[Obj[S]] = {
////      val mId = obj.id.id.toLong << 32
////      implicit val tx = this
////      val mapOpt: Option[AttrMap] = system.tryRead(mId)(SkipList.Map.read[S, String, Obj[S]](_, ()))
////      mapOpt.flatMap(_.get(key))
////    }
//
////    def attrPut(obj: Obj[S], key: String, value: Obj[S]): Unit = {
////      // we could go with one extra bit, but right now there
////      // is already a long-read method, so we'll
////      // use it for now.
////      val mId = obj.id.id.toLong << 32
////      implicit val tx = this
////      val map: AttrMap = system.tryRead(mId)(SkipList.Map.read[S, String, Obj[S]](_, ()))
////        .getOrElse(SkipList.Map.empty[S, String, Obj[S]])
////      map.add(key -> value)
////    }
//
////    def attrRemove(obj: Obj[S], key: String): Unit = {
////      val mId = obj.id.id.toLong << 32
////      implicit val tx = this
////      val mapOpt: Option[AttrMap] = system.tryRead(mId)(SkipList.Map.read[S, String, Obj[S]](_, ()))
////      mapOpt.foreach { map =>
////        map.remove(key)
////        if (map.isEmpty) system.remove(mId)
////      }
////    }
////
////    def attrIterator(obj: Obj[S]): Iterator[(String, Obj[S])] = {
////      val mId = obj.id.id.toLong << 32
////      implicit val tx = this
////      val mapOpt: Option[AttrMap] = system.tryRead(mId)(SkipList.Map.read[S, String, Obj[S]](_, ()))
////      mapOpt.fold[Iterator[(String, Obj[S])]](Iterator.empty)(_.iterator)
////    }
//  }
//
//  private final class IdImpl[S <: D[S]](val id: Int) extends DurableLike.Id[S] {
//    def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)
//
//    override def hashCode: Int = id
//
//    override def equals(that: Any): Boolean = that match {
//      case b: IdImpl[_] => id === b.id
//      case _ => false
//    }
//
//    def dispose()(implicit tx: S#Tx): Unit = tx.system.remove(id)
//
//    override def toString = s"<$id>"
//  }
//
////  private final class IdMapImpl[S <: D[S], A](val id: S#Id)(implicit serializer: Serializer[S#Tx, S#Acc, A])
////    extends IdentifierMap[S#Id, S#Tx, A] {
////    map =>
////
////    private[this] val idn = id.id.toLong << 32
////
////    def get(id: S#Id)(implicit tx: S#Tx): Option[A] = {
////      tx.system.tryRead(idn | (id.id.toLong & 0xFFFFFFFFL))(serializer.read(_, ()))
////    }
////
////    def getOrElse(id: S#Id, default: => A)(implicit tx: S#Tx): A = get(id).getOrElse(default)
////
////    def put(id: S#Id, value: A)(implicit tx: S#Tx): Unit =
////      tx.system.write(idn | (id.id.toLong & 0xFFFFFFFFL))(serializer.write(value, _))
////
////    def contains(id: S#Id)(implicit tx: S#Tx): Boolean =
////      tx.system.exists(idn | (id.id.toLong & 0xFFFFFFFFL))
////
////    def remove(id: S#Id)(implicit tx: S#Tx): Unit =
////      tx.system.remove(idn | (id.id.toLong & 0xFFFFFFFFL))
////
////    def write(out: DataOutput): Unit = id.write(out)
////
////    def dispose()(implicit tx: S#Tx): Unit = id.dispose()
////
////    override def toString = s"IdentifierMap$id"
////  }
//
//  private abstract class BasicSource[S <: D[S], A] extends Var[S#Tx, A] {
//    protected def id: Int
//
//    final def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)
//
//    final def dispose()(implicit tx: S#Tx): Unit = tx.system.remove(id)
//
//    @elidable(elidable.CONFIG) protected final def assertExists()(implicit tx: S#Tx): Unit =
//      require(tx.system.exists(id), s"trying to write disposed ref $id")
//  }
//
//  private final class VarImpl[S <: D[S], A](protected val id: Int,
//                                            protected val ser: Serializer[S#Tx, S#Acc, A])
//    extends BasicSource[S, A] {
//
//    def apply()(implicit tx: S#Tx): A =
//      tx.system.read[A](id)(ser.read(_, ()))
//
//    def setInit(v: A)(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(ser.write(v, _))
//
//    def update(v: A)(implicit tx: S#Tx): Unit = {
//      assertExists()
//      tx.system.write(id)(ser.write(v, _))
//    }
//
//    def swap(v: A)(implicit tx: S#Tx): A = {
//      val res = this()
//      this() = v
//      res
//    }
//
//    override def toString = s"Var($id)"
//  }
//
//  private final class CachedVarImpl[S <: D[S], A](protected val id: Int, peer: Ref[A],
//                                                  ser: Serializer[S#Tx, S#Acc, A])
//    extends BasicSource[S, A] {
//
//    def apply()(implicit tx: S#Tx): A = peer.get(tx.peer)
//
//    def setInit(v: A)(implicit tx: S#Tx): Unit = this() = v
//
//    def update(v: A)(implicit tx: S#Tx): Unit = {
//      peer.set(v)(tx.peer)
//      tx.system.write(id)(ser.write(v, _))
//    }
//
//    def writeInit()(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(ser.write(this(), _))
//
//    def readInit()(implicit tx: S#Tx): Unit =
//      peer.set(tx.system.read(id)(ser.read(_, ())))(tx.peer)
//
//    def swap(v: A)(implicit tx: S#Tx): A = {
//      val res = peer.swap(v)(tx.peer)
//      tx.system.write(id)(ser.write(v, _))
//      res
//    }
//
//    override def toString = s"Var($id)"
//  }
//
//  private final class BooleanVar[S <: D[S]](protected val id: Int)
//    extends BasicSource[S, Boolean] {
//
//    def apply()(implicit tx: S#Tx): Boolean =
//      tx.system.read[Boolean](id)(_.readBoolean())
//
//    def setInit(v: Boolean)(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(_.writeBoolean(v))
//
//    def update(v: Boolean)(implicit tx: S#Tx): Unit = {
//      assertExists()
//      tx.system.write(id)(_.writeBoolean(v))
//    }
//
//    def swap(v: Boolean)(implicit tx: S#Tx): Boolean = {
//      val res = this()
//      this() = v
//      res
//    }
//
//    override def toString = s"Var[Boolean]($id)"
//  }
//
//  private final class IntVar[S <: D[S]](protected val id: Int)
//    extends BasicSource[S, Int] {
//
//    def apply()(implicit tx: S#Tx): Int =
//      tx.system.read[Int](id)(_.readInt())
//
//    def setInit(v: Int)(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(_.writeInt(v))
//
//    def update(v: Int)(implicit tx: S#Tx): Unit = {
//      assertExists()
//      tx.system.write(id)(_.writeInt(v))
//    }
//
//    def swap(v: Int)(implicit tx: S#Tx): Int = {
//      val res = this()
//      this() = v
//      res
//    }
//
//    override def toString = s"Var[Int]($id)"
//  }
//
//  private final class CachedIntVar[S <: D[S]](protected val id: Int, peer: Ref[Int])
//    extends BasicSource[S, Int] {
//
//    def apply()(implicit tx: S#Tx): Int = peer.get(tx.peer)
//
//    def setInit(v: Int)(implicit tx: S#Tx): Unit = this() = v
//
//    def update(v: Int)(implicit tx: S#Tx): Unit = {
//      peer.set(v)(tx.peer)
//      tx.system.write(id)(_.writeInt(v))
//    }
//
//    def writeInit()(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(_.writeInt(this()))
//
//    def readInit()(implicit tx: S#Tx): Unit =
//      peer.set(tx.system.read(id)(_.readInt()))(tx.peer)
//
//    def swap(v: Int)(implicit tx: S#Tx): Int = {
//      val res = peer.swap(v)(tx.peer)
//      tx.system.write(id)(_.writeInt(v))
//      res
//    }
//
//    override def toString = s"Var[Int]($id)"
//  }
//
//  private final class LongVar[S <: D[S]](protected val id: Int)
//    extends BasicSource[S, Long] {
//
//    def apply()(implicit tx: S#Tx): Long =
//      tx.system.read[Long](id)(_.readLong())
//
//    def setInit(v: Long)(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(_.writeLong(v))
//
//    def update(v: Long)(implicit tx: S#Tx): Unit = {
//      assertExists()
//      tx.system.write(id)(_.writeLong(v))
//    }
//
//    def swap(v: Long)(implicit tx: S#Tx): Long = {
//      val res = this()
//      this() = v
//      res
//    }
//
//    override def toString = s"Var[Long]($id)"
//  }
//
//  private final class CachedLongVar[S <: D[S]](protected val id: Int, peer: Ref[Long])
//    extends BasicSource[S, Long] {
//
//    def apply()(implicit tx: S#Tx): Long = peer.get(tx.peer)
//
//    def setInit(v: Long)(implicit tx: S#Tx): Unit = this() = v
//
//    def update(v: Long)(implicit tx: S#Tx): Unit = {
//      peer.set(v)(tx.peer)
//      tx.system.write(id)(_.writeLong(v))
//    }
//
//    def writeInit()(implicit tx: S#Tx): Unit =
//      tx.system.write(id)(_.writeLong(this()))
//
//    def readInit()(implicit tx: S#Tx): Unit =
//      peer.set(tx.system.read(id)(_.readLong()))(tx.peer)
//
//    def swap(v: Long)(implicit tx: S#Tx): Long = {
//      val res = peer.swap(v)(tx.peer)
//      tx.system.write(id)(_.writeLong(v))
//      res
//    }
//
//    override def toString = s"Var[Long]($id)"
//  }
//
//  private final class TxnImpl(val system: System, val peer: InTxn)
//    extends TxnMixin[Durable] with Durable.Txn {
//
//    lazy val inMemory: InMemory#Tx = system.inMemory.wrap(peer)
//
//    override def toString = s"Durable.Txn@${hashCode.toHexString}"
//  }
//
//  private final class System(val store: DataStore)
//    extends Mixin[Durable, InMemory] with Durable {
//
//    private type S = Durable    // scalac bug -- it _is_ used
//
//    val inMemory: InMemory = InMemory()
//
//    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory
//
//    override def toString = s"Durable@${hashCode.toHexString}"
//
//    def wrap(peer: InTxn, systemTimeNanos: Long): S#Tx = new TxnImpl(this, peer)
//  }
//}
