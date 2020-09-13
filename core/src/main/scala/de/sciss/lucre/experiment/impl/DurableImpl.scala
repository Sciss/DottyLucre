/*
 *  DurableImpl.scala
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
import de.sciss.lucre.experiment.Log.logTxn
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.elidable
import scala.concurrent.stm.{InTxn, Ref}

object DurableImpl {
  private type D[T <: DurableLike.Txn[T]] = DurableLike.Txn[T]

  def apply(factory: DataStore.Factory, mainName: String): Durable = {
    val mainStore   = factory.open(mainName)
    apply(mainStore = mainStore)
  }

  def apply(mainStore: DataStore): Durable = new System(store = mainStore)

  trait Mixin[T <: D[T], I <: Txn[I]] extends DurableLike[T] with ReactionMapImpl.Mixin[T] {
    self =>

    def store: DataStore

    protected final val eventMap: IdentMap[Ident[T], T, Map[Int, scala.List[Observer[T, _]]]] =
      IdentMapImpl[Ident[T], T, Map[Int, scala.List[Observer[T, _]]]] { implicit tx => id => id.!.id }

    private[this] val idCntVar = step { implicit tx =>
      val _id = store.get(_.writeInt(0))(_.readInt()).getOrElse(1)
      val _idCnt = Ref(_id)
      new CachedIntVar[T](tx)(0, _idCnt)
    }

    def root[A](init: T => A)(implicit serializer: TSerializer[T, A]): Handle[T, A] =
      step { implicit tx =>
        rootBody(init)
      }

    def rootJoin[A](init: T => A)(implicit tx: TxnLike, serializer: TSerializer[T, A]): Handle[T, A] =
      rootBody(init)(wrap(tx.peer), serializer)

    private[this] def rootBody[A](init: T => A)
                                 (implicit tx: T, serializer: TSerializer[T, A]): Handle[T, A] = {
      val rootId = 2 // 1 == reaction map!!!
      if (exists(rootId)) {
        new VarImpl[T, A](tx)(rootId, serializer)
      } else {
        val id = newIdValue()
        require(id === rootId,
          s"Root can only be initialized on an empty database (expected id count is $rootId but found $id)")
        val res = new VarImpl[T, A](tx)(id, serializer)
        res.setInit(init(tx))
        res
      }
    }

    // ---- cursor ----

    def step[A](fun: T => A): A = stepTag(0L)(fun)

    def stepTag[A](systemTimeNanos: Long)(fun: T => A): A = Txn.atomic { implicit itx =>
      fun(wrap(itx, systemTimeNanos))
    }

    def position(implicit tx: T): Unit = ()

    def debugListUserRecords()(implicit tx: T): Seq[Id] = {
      val b   = Seq.newBuilder[Id]
      val cnt = idCntVar()
      var i   = 1
      while (i <= cnt) {
        if (exists(i)) b += new IdImpl[T](tx)(i)
        i += 1
      }
      b.result()
    }

    def close(): Unit = store.close()

    def numRecords(implicit tx: T): Int = store.numEntries

    def numUserRecords(implicit tx: T): Int = math.max(0, numRecords - 1)

    // this increases a durable variable, thus ensures markDirty() already
    def newIdValue()(implicit tx: T): Int = {
      val id = idCntVar() + 1
      logTxn(s"new   <$id>")
      idCntVar() = id
      id
    }

    def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: T): Unit = {
      logTxn(s"writeL <$id>")
      store.put(_.writeLong(id))(valueFun)
    }

    def write(id: Int)(valueFun: DataOutput => Unit)(implicit tx: T): Unit = {
      logTxn(s"write <$id>")
      store.put(_.writeInt(id))(valueFun)
    }

    def remove(id: Long)(implicit tx: T): Unit = {
      logTxn(s"removL <$id>")
      store.remove(_.writeLong(id))
    }

    def remove(id: Int)(implicit tx: T): Unit = {
      logTxn(s"remov <$id>")
      store.remove(_.writeInt(id))
      //         tx.markDirty()
    }

    def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: T): Option[A] = {
      logTxn(s"readL  <$id>")
      store.get(_.writeLong(id))(valueFun)
    }

    def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: T): A = {
      logTxn(s"read  <$id>")
      store.get(_.writeInt(id))(valueFun).getOrElse(sys.error(s"Key not found $id"))
    }

    def exists(id: Int)(implicit tx: T): Boolean = store.contains(_.writeInt(id))

    def exists(id: Long)(implicit tx: T): Boolean = store.contains(_.writeLong(id))
  }

  trait TxnMixin[T <: D[T]] extends DurableLike.Txn[T] with BasicTxnImpl[T] {
    self: T =>

    private[experiment] final def reactionMap: ReactionMap[T] = ??? // system.reactionMap

    final def newId(): Id = new IdImpl[T](this)(system.newIdValue()(this))
    
//    final def newCachedVar[A](init: A)(implicit ser: TSerializer[T, A]): Var[A] = {
//      val res = new CachedVarImpl[T, A](system.newIdValue()(this), Ref(init), ser)
//      res.writeInit() // (this)
//      res
//    }
//
//    final def newCachedIntVar(init: Int): Var[Int] = {
//      val res = new CachedIntVar[T](system.newIdValue()(this), Ref(init))
//      res.writeInit() // (this)
//      res
//    }
//    
//    final def newCachedLongVar(init: Long): Var[Long] = {
//      val res = new CachedLongVar[T](system.newIdValue()(this), Ref(init))
//      res.writeInit() // (this)
//      res
//    }

    final def newVarArray[A](size: Int): Array[Var[A]] = new Array[Var[A]](size)

    final def newInMemoryIdMap[A]: IdentMap[Ident[T], T, A] =
      IdentMapImpl[Ident[T], T, A] { implicit tx => id => id.!.id }

//    final def readVar[A](pid: T#Id, in: DataInput)(implicit ser: TSerializer[T, A]): Var[A] = {
//      val id = in./* PACKED */ readInt()
//      new VarImpl[T, A](id, ser)
//    }
//
//    final def readCachedVar[A](in: DataInput)(implicit ser: TSerializer[T, A]): Var[A] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedVarImpl[T, A](id, Ref.make[A](), ser)
//      res.readInit() // (this)
//      res
//    }
//
//    final def readBooleanVar(pid: T#Id, in: DataInput): Var[Boolean] = {
//      val id = in./* PACKED */ readInt()
//      new BooleanVar[T](id)
//    }
//
//    final def readIntVar(pid: T#Id, in: DataInput): Var[Int] = {
//      val id = in./* PACKED */ readInt()
//      new IntVar[T](id)
//    }

//    final def readCachedIntVar(in: DataInput): Var[Int] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedIntVar[T](id, Ref(0))
//      res.readInit() // (this)
//      res
//    }
//
//    final def readLongVar(pid: T#Id, in: DataInput): Var[Long] = {
//      val id = in./* PACKED */ readInt()
//      new LongVar[T](id)
//    }
//
//    final def readCachedLongVar(in: DataInput): Var[Long] = {
//      val id = in./* PACKED */ readInt()
//      val res = new CachedLongVar[T](id, Ref(0L))
//      res.readInit()(this)
//      res
//    }

    final def readId(in: DataInput, acc: Acc): Id = {
      val base = in./* PACKED */ readInt()
      new IdImpl[T](this)(base)
    }

    final def newHandle[A](value: A)(implicit serializer: TSerializer[T, A]): Handle[T, A] =
      new EphemeralHandle(value)

    // ---- attributes ----

    def attrMap(obj: Obj[T]): Obj.AttrMap[T] = {
      val mId = obj.id.!.id.toLong << 32
      implicit val tx: T = this
      implicit val acc: Acc = ()
      val mapOpt: Option[Obj.AttrMap[T]] = system.tryRead(mId)(TMap.Modifiable.read[T, String, Obj](_, tx))
      mapOpt.getOrElse {
        val map = TMap.Modifiable[T, String, Obj]()
        system.write(mId)(map.write)
        map
      }
    }

    override def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]] = {
      val mId = obj.id.!.id.toLong << 32
      implicit val tx: T = this
      implicit val acc: Acc = ()
      system.tryRead(mId)(TMap.Modifiable.read[T, String, Obj](_, tx))
    }
  }

  private final class IdImpl[T <: D[T]](tx: T)(val id: Int) extends DurableLike.Id[T] {
    def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)

    override def hashCode: Int = id

    override def equals(that: Any): Boolean = that match {
      case b: IdImpl[_] => id === b.id
      case _ => false
    }

    def dispose(): Unit = tx.system.remove(id)(tx)

    def newVar[A](init: A)(implicit ser: TSerializer[T, A]): Var[A] = {
      val res = new VarImpl[T, A](tx)(tx.system.newIdValue()(tx), ser)
      res.setInit(init) // (this)
      res
    }

    final def newBooleanVar(init: Boolean): Var[Boolean] = {
      val res = new BooleanVar[T](tx)(tx.system.newIdValue()(tx))
      res.setInit(init) // (this)
      res
    }

    final def newIntVar(init: Int): Var[Int] = {
      val res = new IntVar[T](tx)(tx.system.newIdValue()(tx))
      res.setInit(init) // (this)
      res
    }

    final def newLongVar(init: Long): Var[Long] = {
      val res = new LongVar[T](tx)(tx.system.newIdValue()(tx))
      res.setInit(init) // (this)
      res
    }

    override def toString = s"<$id>"
  }

  private abstract class BasicHandle[T <: D[T], A](tx: T) extends Var[A] with Handle[T, A] {
    protected def id: Int

    def apply()(implicit tx: T): A = apply()

    final def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)

    final def dispose(): Unit = tx.system.remove(id)(tx)

    @elidable(elidable.CONFIG) protected final def assertExists(): Unit =
      require(tx.system.exists(id)(tx), s"trying to write disposed ref $id")
  }

  private final class VarImpl[T <: D[T], A](tx: T)
                                           (protected val id: Int,
                                            protected val ser: TSerializer[T, A])
    extends BasicHandle[T, A](tx) {

    def apply(): A =
      tx.system.read[A](id)(ser.read(_, tx)(()))(tx)

    def setInit(v: A): Unit =
      tx.system.write(id)(ser.write(v, _))(tx)

    def update(v: A): Unit = {
      assertExists()
      tx.system.write(id)(ser.write(v, _))(tx)
    }

    def swap(v: A): A = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var($id)"
  }

  private final class CachedVarImpl[T <: D[T], A](tx: T)
                                                 (protected val id: Int, peer: Ref[A],
                                                  ser: TSerializer[T, A])
    extends BasicHandle[T, A](tx) {

    def apply(): A = peer.get(tx.peer)

    def setInit(v: A): Unit = this() = v

    def update(v: A): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(ser.write(v, _))(tx)
    }

    def writeInit(): Unit =
      tx.system.write(id)(ser.write(this(), _))(tx)

    def readInit(): Unit =
      peer.set(tx.system.read(id)(ser.read(_, tx)(()))(tx))(tx.peer)

    def swap(v: A): A = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(ser.write(v, _))(tx)
      res
    }

    override def toString = s"Var($id)"
  }

  private final class BooleanVar[T <: D[T]](tx: T)(protected val id: Int)
    extends BasicHandle[T, Boolean](tx) {

    def apply(): Boolean =
      tx.system.read[Boolean](id)(_.readBoolean())(tx)

    def setInit(v: Boolean): Unit =
      tx.system.write(id)(_.writeBoolean(v))(tx)

    def update(v: Boolean): Unit = {
      assertExists()
      tx.system.write(id)(_.writeBoolean(v))(tx)
    }

    def swap(v: Boolean): Boolean = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var[Boolean]($id)"
  }

  private final class IntVar[T <: D[T]](tx: T)(protected val id: Int)
    extends BasicHandle[T, Int](tx) {

    def apply(): Int =
      tx.system.read[Int](id)(_.readInt())(tx)

    def setInit(v: Int): Unit =
      tx.system.write(id)(_.writeInt(v))(tx)

    def update(v: Int): Unit = {
      assertExists()
      tx.system.write(id)(_.writeInt(v))(tx)
    }

    def swap(v: Int): Int = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var[Int]($id)"
  }

  private final class CachedIntVar[T <: D[T]](tx: T)(protected val id: Int, peer: Ref[Int])
    extends BasicHandle[T, Int](tx) {

    def apply(): Int = peer.get(tx.peer)

    def setInit(v: Int): Unit = this() = v

    def update(v: Int): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(_.writeInt(v))(tx)
    }

    def writeInit(): Unit =
      tx.system.write(id)(_.writeInt(this()))(tx)

    def readInit(): Unit =
      peer.set(tx.system.read(id)(_.readInt())(tx))(tx.peer)

    def swap(v: Int): Int = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(_.writeInt(v))(tx)
      res
    }

    override def toString = s"Var[Int]($id)"
  }

  private final class LongVar[T <: D[T]](tx: T)(protected val id: Int)
    extends BasicHandle[T, Long](tx) {

    def apply(): Long =
      tx.system.read[Long](id)(_.readLong())(tx)

    def setInit(v: Long): Unit =
      tx.system.write(id)(_.writeLong(v))(tx)

    def update(v: Long): Unit = {
      assertExists()
      tx.system.write(id)(_.writeLong(v))(tx)
    }

    def swap(v: Long): Long = {
      val res = this()
      this() = v
      res
    }

    override def toString = s"Var[Long]($id)"
  }

  private final class CachedLongVar[T <: D[T]](tx: T)(protected val id: Int, peer: Ref[Long])
    extends BasicHandle[T, Long](tx) {

    def apply(): Long = peer.get(tx.peer)

    def setInit(v: Long): Unit = this() = v

    def update(v: Long): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(_.writeLong(v))(tx)
    }

    def writeInit(): Unit =
      tx.system.write(id)(_.writeLong(this()))(tx)

    def readInit(): Unit =
      peer.set(tx.system.read(id)(_.readLong())(tx))(tx.peer)

    def swap(v: Long): Long = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(_.writeLong(v))(tx)
      res
    }

    override def toString = s"Var[Long]($id)"
  }

  private final class TxnImpl(val system: System, val peer: InTxn)
    extends TxnMixin[Durable.Txn] with Durable.Txn {

//    lazy val inMemory: InMemory#Tx = system.inMemory.wrap(peer)

    override def toString = s"Durable.Txn@${hashCode.toHexString}"
  }

  private final class System(val store: DataStore)
    extends Mixin[Durable.Txn, InMemory.Txn] with Durable {

    private type S = Durable    // scalac bug -- it _is_ used

    val inMemory: InMemory = InMemory()

//    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory

    override def toString = s"Durable@${hashCode.toHexString}"

    def wrap(peer: InTxn, systemTimeNanos: Long): T = ??? // new TxnImpl(this, peer)
  }
}
