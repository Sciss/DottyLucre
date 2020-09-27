/*
 *  Cursor.scala
 *  (Lucre 4)
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

import de.sciss.lucre.confluent.impl.{CursorImpl => Impl}
import de.sciss.lucre.{ConfluentLike, Disposable, DurableLike, Ident => LIdent, Cursor => LCursor, Txn => LTxn, Var => LVar}
import de.sciss.serial.{DataInput, TFormat, Writable}

object Cursor {
  def apply[T <: Txn[T], D1 <: DurableLike.Txn[D1]](init: Access[T] = Access.root[T])
                                                   (implicit tx: D1, system: ConfluentLike[T] { type D = D1 }): Cursor[T, D1] =
    wrap(Data(init))

  def wrap[T <: Txn[T], D1 <: DurableLike.Txn[D1]](data: Data[T, D1])
                                                  (implicit system: ConfluentLike[T] { type D = D1 }): Cursor[T, D1] =
    Impl[T, D1](data)

  def read[T <: Txn[T], D1 <: DurableLike.Txn[D1]](in: DataInput)
                                                  (implicit tx: D1, system: ConfluentLike[T] { type D = D1 }): Cursor[T, D1] =
    Impl.read[T, D1](in)

  implicit def format[T <: Txn[T], D1 <: DurableLike.Txn[D1]](
    implicit system: ConfluentLike[T] { type D = D1 }): TFormat[D1, Cursor[T, D1]] = Impl.format[T, D1]

  object Data {
    def apply[T <: Txn[T], D <: LTxn[D]](init: Access[T] = Access.root[T])(implicit tx: D): Data[T, D] =
      Impl.newData[T, D](init)

    def read[T <: Txn[T], D <: LTxn[D]](in: DataInput)(implicit tx: D): Data[T, D] =
      Impl.readData[T, D](in)

    implicit def format[T <: Txn[T], D <: LTxn[D]]: TFormat[D, Data[T, D]] =
      Impl.dataFormat[T, D]
  }
  trait Data[T <: Txn[T], D <: LTxn[D]] extends Disposable[D] with Writable {
    def id  : LIdent[D] // D#Id
    def path: LVar[D, Access[T]] // D#Var[S#Acc]
  }
}
trait Cursor[T <: Txn[T], D <: LTxn[D]]
  extends LCursor[T] with Disposable[D] with Writable {

  def data: Cursor.Data[T, D]

  def stepFrom[A](path: Access[T], retroactive: Boolean = false, systemTimeNanos: Long = 0L)(fun: T => A): A
  
  def positionD(implicit tx: D): Access[T]
}