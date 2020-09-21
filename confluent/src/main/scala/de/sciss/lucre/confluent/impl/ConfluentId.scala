/*
 *  ConfluentId.scala
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

import de.sciss.lucre.TSerializer
import de.sciss.lucre.confluent.Log.log
import de.sciss.serial.{DataInput, DataOutput}

import scala.util.hashing.MurmurHash3

private abstract class IdImpl[T <: Txn[T]] extends Ident[T] {
  type Id = Ident[T]

  protected def tx: T

  final def !(implicit tx: T): Ident[T] = this

  final def dispose(): Unit = ()

  final def write(out: DataOutput): Unit = {
    out./* PACKED */ writeInt(base)
    path.write(out)
  }

  @inline final protected def alloc(): Id = new ConfluentId(tx, tx.system.newIdValue()(tx), path)

  final def newVar[A](init: A)(implicit ser: TSerializer[T, A]): Var[A] = {
    val res = makeVar[A](alloc())
    log(s"txn newVar $res")
    res.setInit(init)
    res
  }

  final def newBooleanVar(init: Boolean): Var[Boolean] = {
    val id  = alloc()
    val res = new BooleanVar(tx, id)
    log(s"txn newVar $res")
    res.setInit(init)
    res
  }

  final def newIntVar(init: Int): Var[Int] = {
    val id  = alloc()
    val res = new IntVar(tx, id)
    log(s"txn newVar $res")
    res.setInit(init)
    res
  }

  final def newLongVar(init: Long): Var[Long] = {
    val id  = alloc()
    val res = new LongVar(tx, id)
    log(s"txn newVar $res")
    res.setInit(init)
    res
  }

  private def makeVar[A](id: Id)(implicit ser: TSerializer[T, A]): BasicVar[T, A ] = {
    // XXX TODO
    ser match {
//      case plain: NewImmutSerializer[_] =>
//        new VarImpl[T, A](this, id, plain.asInstanceOf[NewImmutSerializer[A]])
      case _ =>
        new VarTxImpl[T, A](tx, id)
    }
  }

  final def readVar[A](in: DataInput)(implicit ser: TSerializer[T, A]): Var[A] = {
    val res = makeVar[A](readSource(in))
    log(s"txn read $res")
    res
  }


  final protected def readSource(in: DataInput): Id = {
    val base = in./* PACKED */ readInt()
    new ConfluentId(tx, base, path)
  }

  final protected def readPartialSource(in: DataInput): Id = {
    val base = in./* PACKED */ readInt()
    new PartialId(tx, base, path)
  }

  //  final def readPartialVar[A](pid: S#Id, in: DataInput)(implicit ser: serial.Serializer[T, S#Acc, A]): S#Var[A] = {
  //    if (Confluent.DEBUG_DISABLE_PARTIAL) return readVar(pid, in)
  //
  //    val res = new PartialVarTxImpl[S, A](readPartialSource(in, pid))
  //    log(s"txn read $res")
  //    res
  //  }

  final def readBooleanVar(in: DataInput): Var[Boolean] = {
    val res = new BooleanVar(tx, readSource(in))
    log(s"txn read $res")
    res
  }

  final def readIntVar(in: DataInput): Var[Int] = {
    val res = new IntVar(tx, readSource(in))
    log(s"txn read $res")
    res
  }

  final def readLongVar(in: DataInput): Var[Long] = {
    val res = new LongVar(tx, readSource(in))
    log(s"txn read $res")
    res
  }
}

private final class ConfluentId[T <: Txn[T]](protected val tx: T, val base: Int, val path: Access[T])
  extends IdImpl[T] {

  override def hashCode: Int = {
    import MurmurHash3._
    val h0  = productSeed
    val h1  = mix(h0, base)
    val h2  = mixLast(h1, path.##)
    finalizeHash(h2, 2)
  }

  def copy(newPath: Access[T]): Ident[T] = new ConfluentId(tx, base = base, path = newPath)

  override def equals(that: Any): Boolean = that match {
    case b: Ident[_] => base == b.base && path == b.path
    case _ => false
  }

  override def toString: String = path.mkString(s"<$base @ ", ",", ">")
}

private final class PartialId[T <: Txn[T]](protected val tx: T, val base: Int, val path: Access[T])
  extends IdImpl[T] {

  override def hashCode: Int = {
    import MurmurHash3._
    val h0  = productSeed
    if (path.isEmpty) {
      val h1  = mixLast(h0, base)
      finalizeHash(h1, 1)
    } else {
      val h1  = mix    (h0, base)
      val h2  = mix    (h1, (path.head >> 32).toInt)
      val h3  = mixLast(h2, (path.last >> 32).toInt)
      finalizeHash(h3, 3)
    }
  }

  def copy(newPath: Access[T]): Ident[T] = new PartialId(tx, base = base, path = newPath)

    override def equals(that: Any): Boolean =
      that match {
        case b: PartialId[_] =>
          val bp: PathLike = b.path
          if (path.isEmpty) {
            base == b.base && bp.isEmpty
          } else {
            base == b.base && bp.nonEmpty && path.head == bp.head && path.last == bp.last
          }

        case _ => false
      }

  override def toString: String = {
    val tail = if (path.isEmpty) ""
    else {
      val head = path.head
      val tail = path.tail
      val (mid, last) = tail.splitIndex
      mid.mkString(s"${head.toInt}(,", ",", s"),${last.toInt}")
    }
    s"<$base @ $tail>"
  }
}
