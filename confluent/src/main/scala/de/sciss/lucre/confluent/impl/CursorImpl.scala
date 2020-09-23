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
import de.sciss.lucre.{Ident => LIdent, Txn => LTxn, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

object CursorImpl {
  private final val COOKIE  = 0x4375  // "Cu"

  implicit def format[T <: Txn[T], D1 <: DurableLike.Txn[D1]](implicit system:
    ConfluentLike[T] { type D = D1 }): TFormat[D1, Cursor[T, D1]] = new Fmt[T, D1]

  private final class Fmt[T <: Txn[T], D1 <: DurableLike.Txn[D1]](implicit system: ConfluentLike[T] { type D = D1 })
    extends WritableFormat[D1, Cursor[T, D1]] {

    override def readT(in: DataInput)(implicit tx: D1): Cursor[T, D1] =
      CursorImpl.read[T, D1](in)
  }

//  private trait NoSys extends Sys[NoSys] { type D = stm.Durable }
//  private type NoSys = Sys

  /* implicit */ def pathFormat[T <: Txn[T], D <: LTxn[D]]: TFormat[D, Access[T]] =
    anyPathFmt.asInstanceOf[PathFmt[T, D]]

  private final val anyPathFmt = new PathFmt[Confluent.Txn, AnyTxn]

  private final class PathFmt[T <: Txn[T], D1 <: LTxn[D1]] // (implicit system: S { type D = D1 })
    extends WritableFormat[D1, Access[T]] {

    override def readT(in: DataInput)(implicit tx: D1): Access[T] =
      confluent.Access.read(in) // system.readPath(in)
  }

  def newData[T <: Txn[T], D <: LTxn[D]](init: Access[T] = Access.root[T])(implicit tx: D): Data[T, D] = {
    val id    = tx.newId()
    val path  = id.newVar(init)(tx, pathFormat[T, D])
    new DataImpl[T, D](id, path)
  }

  def dataFormat[T <: Txn[T], D <: LTxn[D]]: TFormat[D, Data[T, D]] =
    anyDataFmt.asInstanceOf[DataFmt[T, D]]

  private final val anyDataFmt = new DataFmt[Confluent.Txn, AnyTxn]

  private final class DataFmt[T <: Txn[T], D <: LTxn[D]]
    extends WritableFormat[D, Data[T, D]] {

    override def readT(in: DataInput)(implicit tx: D): Data[T, D] = readData[T, D](in)
  }

  def readData[T <: Txn[T], D <: LTxn[D]](in: DataInput)(implicit tx: D): Data[T, D] = {
    val cookie  = in.readShort()
    if (cookie != COOKIE) throw new IllegalStateException(s"Unexpected cookie $cookie (should be $COOKIE)")
    val id      = tx.readId(in) // implicitly[Format[D#Tx, D#Acc, D#Id]].read(in)
    val path    = id.readVar[Access[T]](in)(tx, pathFormat[T, D])
    new DataImpl[T, D](id, path)
  }

  private final class DataImpl[T <: Txn[T], D <: LTxn[D]](val id: LIdent[D], val path: LVar[D, Access[T]])
    extends Data[T, D] {

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      id  .write(out)
      path.write(out)
    }

    def dispose()(implicit tx: D): Unit = {
      path.dispose()
      id  .dispose()
    }
  }

  def apply[T <: Txn[T], D1 <: DurableLike.Txn[D1]](data: Data[T, D1])
                                                   (implicit system: ConfluentLike[T] { type D = D1 }): Cursor[T, D1] =
    new Impl[T, D1](data)

  def read[T <: Txn[T], D1 <: DurableLike.Txn[D1]](in: DataInput)(implicit tx: D1,
                                                                  system: ConfluentLike[T] { type D = D1 }): Cursor[T, D1] = {
    val data = readData[T, D1](in)
    Cursor.wrap[T, D1](data)
  }

  private final class Impl[T <: Txn[T], D1 <: DurableLike.Txn[D1]](val data: Data[T, D1])
                                                                  (implicit system: ConfluentLike[T] { type D = D1 })
    extends Cursor[T, D1] with Cache[T] {

    override def toString = s"Cursor${data.id}"

    private def topLevelAtomic[A](fun: D1 => A): A = Txn.atomic { itx =>
      val dtx: D1 = system.durable.wrap(itx)
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
      val tx: T = system.createTxn(dtx = dtx, inputAccess = inputAccess, retroactive = retroactive,
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

    def position  (implicit tx: T ): Access[T]  = positionD(system.durableTx(tx))
    def positionD (implicit tx: D1): Access[T]  = data.path()

    def dispose()(implicit tx: D1): Unit = {
      data.dispose()
//      id  .dispose()
//      path.dispose()
      logCursor(s"${data.id} dispose")
    }

    def write(out: DataOutput): Unit = data.write(out)
  }
}