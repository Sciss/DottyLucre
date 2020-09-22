/*
 *  VarImpl.scala
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
import de.sciss.lucre.{ConstantSerializer, TSerializer}
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.LongMap

private[impl] final class HandleImpl[T <: Txn[T], A](stale: A, writeIndex: Access[T])
                                              (implicit serializer: TSerializer[T, A])
  extends TSource[T, A] with Cache[T] {

  private var writeTerm = 0L

  override def toString = s"handle: $stale"

  def flushCache(term: Long)(implicit tx: T): Unit =
    writeTerm = term

  def meld(from: Access[T])(implicit tx: T): A = {
    if (writeTerm == 0L) throw new IllegalStateException(s"Cannot meld a handle that was not yet flushed: $this")
    log(s"$this meld $from")
    tx.addInputVersion(from)
    apply1(from)
  }

  def apply()(implicit tx: T): A = {
    if (writeTerm == 0L) return stale // wasn't flushed yet
    apply1(tx.inputAccess)
  }

  private def apply1(readPath: Access[T])(implicit tx: T): A = {
    val out = DataOutput()
    serializer.write(stale, out)
    val in = DataInput(out.buffer, 0, out.size)

    var entries = LongMap.empty[Long]
    Hashing.foreachPrefix(writeIndex, entries.contains) {
      case (_hash, _preSum) => entries += ((_hash, _preSum))
    }
    entries += ((writeIndex.sum, 0L)) // full cookie

    var (maxIndex, maxTerm) = readPath.splitIndex
    while (true) {
      val preLen = Hashing.maxPrefixLength(maxIndex, entries.contains)
      val index = if (preLen == maxIndex.size) {
        // maximum prefix lies in last tree
        maxIndex
      } else {
        // prefix lies in other tree
        maxIndex.take(preLen)
      }
      val preSum = index.sum
      val hash = entries(preSum)
      if (hash == 0L) {
        // full entry
        val suffix = writeTerm +: readPath.drop(preLen)
        val res = tx.withReadAccess(suffix)(serializer.readT(in)(tx))
        return res
      } else {
        // partial hash
        val (fullIndex, fullTerm) = maxIndex.splitAtSum(hash)
        maxIndex = fullIndex
        maxTerm = fullTerm
      }
    }
    sys.error("Never here")
  }
}

private[impl] abstract class BasicVar[T <: Txn[T], A] extends Var[A] {
  protected def tx: T
  protected def id: Ident[T]

  final def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id.base)

  final def swap(v: A): A = {
    val res = apply()
    update(v)
    res
  }

  final def dispose(): Unit = {
    tx.removeFromCache(id)
    id.dispose()
  }

  def setInit(v: A): Unit

//  final def transform(f: A => A): Unit = this() = f(this())
}

private[impl] final class VarImpl[T <: Txn[T], A](protected val tx: T, protected val id: Ident[T],
                                                  protected val ser: ConstantSerializer[A])
  extends BasicVar[T, A] {

  def meld(from: Access[T]): A = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id.base, from)
    tx.addInputVersion(from)
    tx.getNonTxn[A](idm)(ser)
  }

  def update(v: A): Unit = {
    log(s"$this set $v")
    tx.putNonTxn(id, v)(ser)
  }

  def apply(): A = {
    log(s"$this get")
    tx.getNonTxn[A](id)(ser)
  }

  def setInit(v: A): Unit = {
    log(s"$this ini $v")
    tx.putNonTxn(id, v)(ser)
  }

  override def toString = s"Var($id)"
}

private[impl] final class VarTxImpl[T <: Txn[T], A](protected val tx: T, protected val id: Ident[T])
                                                   (implicit ser: TSerializer[T, A])
  extends BasicVar[T, A] {

  def meld(from: Access[T]): A = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id.base, from)
    tx.addInputVersion(from)
    tx.getTxn[A](idm)
  }

  def update(v: A): Unit = {
    log(s"$this set $v")
    tx.putTxn(id, v)
  }

  def apply(): A = {
    log(s"$this get")
    tx.getTxn(id)
  }

  def setInit(v: A): Unit = {
    log(s"$this ini $v")
    tx.putTxn(id, v)
  }

  override def toString = s"Var($id)"
}

private final class RootVar[T <: Txn[T], A](id1: Int, name: String)
                                           (implicit val ser: TSerializer[T, A])
  extends TRef[T, A] {

  def setInit(v: A)(implicit tx: T): Unit = this() = v // XXX could add require( tx.inAccess == Path.root )

  override def toString: String = name // "Root"

  private def id(implicit tx: T): Ident[T] = new ConfluentId[T](tx, id1, tx.inputAccess)

  def meld(from: Access[T])(implicit tx: T): A = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id1, from)
    tx.addInputVersion(from)
    tx.getTxn(idm)(ser)
  }

  def update(v: A)(implicit tx: T): Unit = {
    log(s"$this set $v")
    tx.putTxn(id, v)(ser)
  }

  def apply()(implicit tx: T): A = {
    log(s"$this get")
    tx.getTxn(id)(ser)
  }

//  def swap(v: A)(implicit tx: T): A = {
//    val res = apply()
//    update(v)
//    res
//  }

//  def write(out: DataOutput): Unit =
//    sys.error("Unsupported Operation -- access.write")

//  def dispose()(implicit tx: T): Unit = ()
}

private[impl] final class BooleanVar[T <: Txn[T]](protected val tx: T, protected val id: Ident[T])
  extends BasicVar[T, Boolean] with ConstantSerializer[Boolean] {

  def meld(from: Access[T]): Boolean = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id.base, from)
    tx.addInputVersion(from)
    tx.getNonTxn[Boolean](idm)(this)
  }

  def apply(): Boolean = {
    log(s"$this get")
    tx.getNonTxn[Boolean](id)(this)
  }

  def setInit(v: Boolean): Unit = {
    log(s"$this ini $v")
    tx.putNonTxn(id, v)(this)
  }

  def update(v: Boolean): Unit = {
    log(s"$this set $v")
    tx.putNonTxn(id, v)(this)
  }

  override def toString = s"Var[Boolean]($id)"

  // ---- Serializer ----
  def write(v: Boolean, out: DataOutput): Unit = out.writeBoolean(v)

  def read(in: DataInput): Boolean = in.readBoolean()
}

private[impl] final class IntVar[T <: Txn[T]](protected val tx: T, protected val id: Ident[T])
  extends BasicVar[T, Int] with ConstantSerializer[Int] {

  def meld(from: Access[T]): Int = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id.base, from)
    tx.addInputVersion(from)
    tx.getNonTxn[Int](idm)
  }

  def apply(): Int = {
    log(s"$this get")
    tx.getNonTxn[Int](id)(this)
  }

  def setInit(v: Int): Unit = {
    log(s"$this ini $v")
    tx.putNonTxn(id, v)(this)
  }

  def update(v: Int): Unit = {
    log(s"$this set $v")
    tx.putNonTxn(id, v)(this)
  }

  override def toString = s"Var[Int]($id)"

  // ---- Serializer ----
  def write(v: Int, out: DataOutput): Unit = out.writeInt(v)

  def read(in: DataInput): Int = in.readInt()
}

private[impl] final class LongVar[T <: Txn[T]](protected val tx: T, protected val id: Ident[T])
  extends BasicVar[T, Long] with ConstantSerializer[Long] {

  def meld(from: Access[T]): Long = {
    log(s"$this meld $from")
    val idm = new ConfluentId[T](tx, id.base, from)
    tx.addInputVersion(from)
    tx.getNonTxn[Long](idm)
  }

  def apply(): Long = {
    log(s"$this get")
    tx.getNonTxn[Long](id)(this)
  }

  def setInit(v: Long): Unit = {
    log(s"$this ini $v")
    tx.putNonTxn(id, v)(this)
  }

  def update(v: Long): Unit = {
    log(s"$this set $v")
    tx.putNonTxn(id, v)(this)
  }

  override def toString = s"Var[Long]($id)"

  // ---- Serializer ----
  def write(v: Long, out: DataOutput): Unit = out.writeLong(v)

  def read(in: DataInput): Long = in.readLong()
}