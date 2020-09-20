/*
 *  CursorImpl.scala
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

package de.sciss.lucre
package confluent
package impl

import de.sciss.lucre.confluent.Cursor.Data
import de.sciss.lucre.confluent.Log.logCursor
import de.sciss.lucre.{confluent, Txn => LTxn, Var => LVar}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object CursorImpl {
  private final val COOKIE  = 0x4375  // "Cu"

  implicit def serializer[T <: Txn[T], D1 <: DurableLike.Txn[D1]]
  /*(implicit system: S { type D = D1 })*/: TSerializer[D1, Cursor[T, D1]] = new Ser[T, D1]

  private final class Ser[T <: Txn[T], D1 <: DurableLike.Txn[D1]]/*(implicit system: S { type D = D1 })*/
    extends serial.Serializer[D1, D1#Acc, Cursor[T, D1]] {

    def write(v: Cursor[T, D1], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: Unit)(implicit tx: D1): Cursor[T, D1] = CursorImpl.read[T, D1](in)
  }

//  private trait NoSys extends Sys[NoSys] { type D = stm.Durable }
  private type NoSys = Sys

  /* implicit */ def pathSerializer[T <: Txn[T], D <: LTxn[D]]: TSerializer[D, Access[T]] =
    anyPathSer.asInstanceOf[PathSer[T, D]]

  private final val anyPathSer = new PathSer[NoSys, NoSys#D]

  private final class PathSer[T <: Txn[T], D1 <: LTxn[D1]] // (implicit system: S { type D = D1 })
    extends TSerializer[D1, Access[T]] {

    def write(v: Access[T], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: D1#Acc)(implicit tx: D1): Access[T] =
      confluent.Access.read(in) // system.readPath(in)
  }

  def newData[T <: Txn[T], D <: LTxn[D]](init: Access[T] = Access.root[T])(implicit tx: D#Tx): Data[T, D] = {
    val id    = tx.newId()
    val path  = tx.newVar(id, init)(pathSerializer[T, D])
    new DataImpl[T, D](id, path)
  }

  def dataSerializer[T <: Txn[T], D <: LTxn[D]]: TSerializer[D, Data[T, D]] =
    anyDataSer.asInstanceOf[DataSer[T, D]]

  private final val anyDataSer = new DataSer[NoSys, NoSys#D]

  private final class DataSer[T <: Txn[T], D <: LTxn[D]]
    extends TSerializer[D, Data[T, D]] {

    def write(d: Data[T, D], out: DataOutput): Unit = d.write(out)

    def read(in: DataInput, tx: T)(implicit access: tx.Acc): Data[T, D] = readData[T, D](in, tx)
  }

  def readData[T <: Txn[T], D <: LTxn[D]](in: DataInput, tx: D)(implicit access: tx.Acc): Data[T, D] = {
    val cookie  = in.readShort()
    if (cookie != COOKIE) throw new IllegalStateException(s"Unexpected cookie $cookie (should be $COOKIE)")
    val id      = tx.readId(in) // implicitly[Serializer[D#Tx, D#Acc, D#Id]].read(in)
    val path    = id.readVar[Access[T]](in)(pathSerializer[T, D])
    new DataImpl[T, D](id, path)
  }

  private final class DataImpl[T <: Txn[T], D <: LTxn[D]](val id: D#Id, val path: LVar[Access[T]])
    extends Data[T, D] {

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      id  .write(out)
      path.write(out)
    }

    def dispose()(implicit tx: D#Tx): Unit = {
      path.dispose()
      id  .dispose()
    }
  }

  def apply[T <: Txn[T], D1 <: DurableLike.Txn[D1]](data: Data[T, D1])
                                                  /*(implicit system: S { type D = D1 })*/: Cursor[T, D1] =
    new Impl[T, D1](data)

  def read[T <: Txn[T], D1 <: DurableLike.Txn[D1]](in: DataInput)
                                                  (implicit tx: D1 /*, system: S { type D = D1 }*/): Cursor[T, D1] = {
    val data = readData[T, D1](in, ())
    Cursor.wrap[T, D1](data)
  }

  private final class Impl[T <: Txn[T], D1 <: DurableLike.Txn[D1]](val data: Data[T, D1])
                                                          /*(implicit system: S { type D = D1 })*/
    extends Cursor[T, D1] with Cache[T] {

    override def toString = s"Cursor${data.id}"

    private def topLevelAtomic[A](fun: D1 => A): A = Txn.atomic { itx =>
      val dtx = system.durable.wrap(itx)
      fun(dtx)
    }

    def step[A](fun: T => A): A = stepTag(0L)(fun)

    def stepTag[A](systemTimeNanos: Long)(fun: T => A): A = {
      topLevelAtomic { implicit dtx =>
        val inputAccess = data.path()
        performStep(inputAccess, systemTimeNanos = systemTimeNanos, retroactive = false, dtx = dtx, fun = fun)
      }
    }

    def stepFrom[A](inputAccess: Access[T], retroactive: Boolean, systemTimeNanos: Long)(fun: T => A): A = {
      topLevelAtomic { implicit dtx =>
        data.path() = inputAccess
        performStep(inputAccess, systemTimeNanos = systemTimeNanos, retroactive = retroactive, dtx = dtx, fun = fun)
      }
    }

    private def performStep[A](inputAccess: Access[T], retroactive: Boolean, systemTimeNanos: Long,
                               dtx: D1, fun: T => A): A = {
      val tx = system.createTxn(dtx = dtx, inputAccess = inputAccess, retroactive = retroactive,
        cursorCache = this, systemTimeNanos = systemTimeNanos)
      logCursor(s"${data.id} step. input path = $inputAccess")
      fun(tx)
    }

    def flushCache(term: Long)(implicit tx: T): Unit = {
      implicit val dtx: D1 = system.durableTx(tx)
      val newPath = tx.inputAccess.addTerm(term)
      data.path() = newPath
      logCursor(s"${data.id} flush path = $newPath")
    }

    def position(implicit tx: T ): Access[T]  = position(system.durableTx(tx))
    def position(implicit tx: D1): Access[T]  = data.path()

    def dispose()(implicit tx: D1): Unit = {
      data.dispose()
//      id  .dispose()
//      path.dispose()
      logCursor(s"${data.id} dispose")
    }

    def write(out: DataOutput): Unit = data.write(out)
  }
}