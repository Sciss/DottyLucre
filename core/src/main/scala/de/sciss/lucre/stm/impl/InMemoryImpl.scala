/*
 *  InMemoryImpl.scala
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

package de.sciss.lucre.stm.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.impl.ReactionMapImpl
import de.sciss.lucre.event.{Observer, ReactionMap}
import de.sciss.lucre.stm.InMemoryLike.Var
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.{Ident, IdentMap, InMemory, InMemoryLike, Obj, Source, TxSerializer, TxnLike}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.{InTxn, TxnExecutor, Ref => ScalaRef}

object InMemoryImpl {
  def apply(): InMemory = new System

  trait Mixin[T <: InMemoryLike.Txn[T]] extends InMemoryLike[T] with ReactionMapImpl.Mixin[T] {
    private[this] final val idCnt = ScalaRef(0)
    
//    protected val idIntView: T => Ident[]

    protected final val eventMap: IdentMap[Ident[T], T, Map[Int, scala.List[Observer[T, _]]]] =
      ??? // IdentMapImpl.newInMemoryIntMap[Ident[T], T, Map[Int, scala.List[Observer[T, _]]]](tx => id => tx.intId(id))

    private[lucre] final val attrMap: IdentMap[Ident[T], T, Obj.AttrMap[T]] =
      ??? // IdentMapImpl.newInMemoryIntMap[Ident[T], T, Obj.AttrMap[T]](tx => id => tx.intId(id))

    private[lucre] final def newIdValue()(implicit tx: T): Int = {
      val peer  = tx.peer
      val res   = idCnt.get(peer) + 1
      idCnt.set(res)(peer)
      res
    }

    final def root[A](init: T => A)(implicit serializer: TxSerializer[T, A]): Source[T, A] =
      step { implicit tx =>
        tx.newVar[A](tx.newId(), init(tx))
      }

    // may nest
    def rootJoin[A](init: T => A)(implicit tx: TxnLike, serializer: TxSerializer[T, A]): Source[T, A] =
      root(init)

    final def close(): Unit = ()

    // ---- cursor ----

    final def step[A](fun: T => A): A = stepTag(0L)(fun)

    final def stepTag[A](systemTimeNanos: Long)(fun: T => A): A = {
      // note: in-memory has no problem with nested
      // transactions, so we do not need to check that condition.
      TxnExecutor.defaultAtomic(itx => fun(wrap(itx, systemTimeNanos)))
    }

    final def position(implicit tx: T): Unit = ()
  }

  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")

  private final class IdImpl[T <: InMemoryLike.Txn[T]](val id: Int) extends Ident[T] /* InMemoryLike.Id[T] */ {
    def write(out: DataOutput): Unit = ()
    def dispose()(implicit tx: T): Unit = ()

    override def toString = s"<$id>"
    override def hashCode: Int    = id.##

    override def equals(that: Any): Boolean = that match {
      case thatId: InMemoryLike.Id[_] => thatId.id === id
      case _ => false
    }
  }

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends TxnMixin[InMemory.Txn] with InMemory.Txn {

//    implicit def inMemory: InMemory#I#Tx = this
//
//    type T  = InMemory.Txn
//    type Id = Ident[T]

    override def toString = s"InMemory.Txn@${hashCode.toHexString}"
  }

  trait TxnMixin[T <: InMemoryLike.Txn[T]] extends BasicTxnImpl[T] with InMemoryLike.Txn[T] {
    self: T =>

    final def newId(): Id = ??? // new IdImpl[T](system.newIdValue()(this))

    final def newHandle[A](value: A)(implicit serializer: TxSerializer[T, A]): Source[T, A] =
      new EphemeralHandle(value)

    private[stm] def getVar[A](vr: Var[A]): A = {
      vr.peer.get(peer)
    }

    private[stm] def putVar[A](vr: Var[A], value: A): Unit = {
      vr.peer.set(value)(peer)
    }

    final def newVar[A](id: Id, init: A)(implicit ser: TxSerializer[T, A]): Var[A] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[T, A](peer)
    }

    final def newIntVar(id: Id, init: Int): Var[Int] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[T, Int](peer)
    }

    final def newBooleanVar(id: Id, init: Boolean): Var[Boolean] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[T, Boolean](peer)
    }

    final def newLongVar(id: Id, init: Long): Var[Long] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[T, Long](peer)
    }

    final def newVarArray[A](size: Int) = new Array[Var[A]](size)

    final def newInMemoryIdMap[A]: IdentMap[Id, T, A] =
      ??? // IdentMapImpl.newInMemoryIntMap[Id, T, A](_.id)

    def readVar[A](id: Id, in: DataInput)(implicit ser: TxSerializer[T, A]): Var[A] =
      opNotSupported("readVar")

    def readBooleanVar(id: Id, in: DataInput): Var[Boolean] = opNotSupported("readBooleanVar")
    def readIntVar    (id: Id, in: DataInput): Var[Int    ] = opNotSupported("readIntVar"    )
    def readLongVar   (id: Id, in: DataInput): Var[Long   ] = opNotSupported("readLongVar"   )

    def readId(in: DataInput)(implicit acc: Acc): Id = opNotSupported("readId")

    private[lucre] final def reactionMap: ReactionMap[T] = system.reactionMap

    // ---- context ----

    // def newContext(): S#Context = new ContextImpl[S]

    // ---- attributes ----

    def attrMap(obj: Obj[T]): Obj.AttrMap[T] = {
      implicit val tx: T = this
      val am  = system.attrMap
      val id  = obj.id
      am.getOrElse(id, {
        val m = evt.Map.Modifiable[T, String, Obj]
        am.put(id, m)
        m
      })
    }

    override def attrMapOption(obj: Obj[T]): Option[AttrMap[T]] = {
      implicit val tx: T = this
      val am  = system.attrMap
      val id  = obj.id
      am.get(id)
    }
  }

  private final class System extends Mixin[InMemory.Txn] with InMemory {
    private type S = InMemory     // scalac bug -- it _is_ used

    def inMemory: I = this
    def inMemoryTx(tx: T): T = tx

    override def toString = s"InMemory@${hashCode.toHexString}"

    def wrap(itx: InTxn, systemTimeNanos: Long): T = new TxnImpl(this, itx)
  }
}