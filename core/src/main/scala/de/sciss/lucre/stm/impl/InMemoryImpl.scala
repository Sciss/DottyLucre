///*
// *  InMemoryImpl.scala
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
//import de.sciss.lucre.stm.InMemoryLike.Var
//import de.sciss.lucre.stm.Obj.AttrMap
//import de.sciss.lucre.stm.{IdentifierMap, InMemory, InMemoryLike, Obj, Source, TxnLike}
//import de.sciss.lucre.{event => evt}
//import de.sciss.serial.{DataInput, DataOutput, Serializer}
//
//import scala.concurrent.stm.{InTxn, TxnExecutor, Ref => ScalaRef}
//
//object InMemoryImpl {
//  def apply(): InMemory = new System
//
//  trait Mixin[S <: InMemoryLike[S]] extends InMemoryLike[S] with ReactionMapImpl.Mixin[S] {
//    private[this] final val idCnt = ScalaRef(0)
//
//    protected final val eventMap: IdentifierMap[S#Id, S#Tx, Map[Int, scala.List[Observer[S, _]]]] =
//      IdentifierMapImpl.newInMemoryIntMap[S#Id, S#Tx, Map[Int, scala.List[Observer[S, _]]]](_.id)
//
//    private[lucre] final val attrMap: IdentifierMap[S#Id, S#Tx, Obj.AttrMap[S]] =
//      IdentifierMapImpl.newInMemoryIntMap[S#Id, S#Tx, Obj.AttrMap[S]](_.id)
//
//    private[lucre] final def newIdValue()(implicit tx: S#Tx): Int = {
//      val peer  = tx.peer
//      val res   = idCnt.get(peer) + 1
//      idCnt.set(res)(peer)
//      res
//    }
//
//    final def root[A](init: S#Tx => A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      step { implicit tx =>
//        tx.newVar[A](tx.newId(), init(tx))
//      }
//
//    // may nest
//    def rootJoin[A](init: S#Tx => A)(implicit tx: TxnLike, serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      root(init)
//
//    final def close(): Unit = ()
//
//    // ---- cursor ----
//
//    final def step[A](fun: S#Tx => A): A = stepTag(0L)(fun)
//
//    final def stepTag[A](systemTimeNanos: Long)(fun: S#Tx => A): A = {
//      // note: in-memory has no problem with nested
//      // transactions, so we do not need to check that condition.
//      TxnExecutor.defaultAtomic(itx => fun(wrap(itx, systemTimeNanos)))
//    }
//
//    final def position(implicit tx: S#Tx): S#Acc = ()
//  }
//
//  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")
//
//  private final class IdImpl[S <: InMemoryLike[S]](val id: Int) extends InMemoryLike.Id[S] {
//    def write(out: DataOutput): Unit = ()
//    def dispose()(implicit tx: S#Tx): Unit = ()
//
//    override def toString = s"<$id>"
//    override def hashCode: Int    = id.##
//
//    override def equals(that: Any): Boolean = that match {
//      case thatId: InMemoryLike.Id[_] => thatId.id === id
//      case _ => false
//    }
//  }
//
////  private final class ContextImpl[S <: InMemoryLike[S]] extends InMemoryLike.Context[S] {
////    private val map = TMap.empty[Any, Any]
////
////    def get[A](vr: Var[S, A])(implicit tx: S#Tx): A = {
////      import TxnLike.peer
////      map.getOrElse(vr, vr.peer.get).asInstanceOf[A]
////    }
////
////    def put[A](vr: Var[S, A], value: A)(implicit tx: S#Tx): Unit = map.put(vr, value)(tx.peer)
////  }
//
//  private final class TxnImpl(val system: InMemory, val peer: InTxn)
//    extends TxnMixin[InMemory] {
//
//    implicit def inMemory: InMemory#I#Tx = this
//
//    override def toString = s"InMemory.Txn@${hashCode.toHexString}"
//  }
//
//  trait TxnMixin[S <: InMemoryLike[S]] extends BasicTxnImpl[S] with InMemoryLike.Txn[S] {
//    _: S#Tx =>
//
//    final def newId(): S#Id = new IdImpl[S](system.newIdValue()(this))
//
//    final def newHandle[A](value: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
//      new EphemeralHandle(value)
//
//    private[stm] def getVar[A](vr: Var[S, A]): A = {
////      if (_context == null)
//        vr.peer.get(peer)
////      else
////        _context.get(vr)(this)
//    }
//
//    private[stm] def putVar[A](vr: Var[S, A], value: A): Unit = {
////      if (_context == null)
//        vr.peer.set(value)(peer)
////      else
////        _context.put(vr, value)(this)
//    }
//
//    final def newVar[A](id: S#Id, init: A)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//      val peer = ScalaRef(init)
//      new SysInMemoryRef[S, A](peer)
//    }
//
//    final def newIntVar(id: S#Id, init: Int): S#Var[Int] = {
//      val peer = ScalaRef(init)
//      new SysInMemoryRef[S, Int](peer)
//    }
//
//    final def newBooleanVar(id: S#Id, init: Boolean): S#Var[Boolean] = {
//      val peer = ScalaRef(init)
//      new SysInMemoryRef[S, Boolean](peer)
//    }
//
//    final def newLongVar(id: S#Id, init: Long): S#Var[Long] = {
//      val peer = ScalaRef(init)
//      new SysInMemoryRef[S, Long](peer)
//    }
//
//    final def newVarArray[A](size: Int) = new Array[S#Var[A]](size)
//
//    final def newInMemoryIdMap[A]: IdentifierMap[S#Id, S#Tx, A] =
//      IdentifierMapImpl.newInMemoryIntMap[S#Id, S#Tx, A](_.id)
//
//    def readVar[A](id: S#Id, in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] =
//      opNotSupported("readVar")
//
//    def readBooleanVar(id: S#Id, in: DataInput): S#Var[Boolean] = opNotSupported("readBooleanVar")
//    def readIntVar    (id: S#Id, in: DataInput): S#Var[Int    ] = opNotSupported("readIntVar"    )
//    def readLongVar   (id: S#Id, in: DataInput): S#Var[Long   ] = opNotSupported("readLongVar"   )
//
//    def readId(in: DataInput, acc: S#Acc): S#Id = opNotSupported("readId")
//
//    private[lucre] final def reactionMap: ReactionMap[S] = system.reactionMap
//
//    // ---- context ----
//
//    // def newContext(): S#Context = new ContextImpl[S]
//
//    // ---- attributes ----
//
//    def attrMap(obj: Obj[S]): Obj.AttrMap[S] = {
//      implicit val tx: S#Tx = this
//      val am  = system.attrMap
//      val id  = obj.id
//      am.getOrElse(id, {
//        val m = evt.Map.Modifiable[S, String, Obj]
//        am.put(id, m)
//        m
//      })
//    }
//
//    override def attrMapOption(obj: Obj[S]): Option[AttrMap[S]] = {
//      implicit val tx: S#Tx = this
//      val am  = system.attrMap
//      val id  = obj.id
//      am.get(id)
//    }
//  }
//
//  private final class System extends Mixin[InMemory] with InMemory {
//    private type S = InMemory     // scalac bug -- it _is_ used
//
//    def inMemory: I = this
//    def inMemoryTx(tx: Tx): Tx = tx
//
//    override def toString = s"InMemory@${hashCode.toHexString}"
//
//    def wrap(itx: InTxn, systemTimeNanos: Long): S#Tx = new TxnImpl(this, itx)
//  }
//}