/*
 *  Elem.scala
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

package de.sciss.lucre.stm

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.{Publisher, Event}
import de.sciss.lucre.stm.impl.{ElemImpl => Impl}
import de.sciss.serial
import de.sciss.serial.{DataInput, Writable}

object Elem {
  def read[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Elem[T] = Impl.read(in, tx)

  implicit def serializer[T <: Txn[T]]: TxSerializer[T, Elem[T]] = Impl.serializer

  trait Type {
    def typeId: Int

    private[this] lazy val _init: Unit = Elem.addType(this)

    def init(): Unit = _init

    def readObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Elem[T] = {
      val tpe = in.readInt()
      if (tpe !== typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
      readIdentifiedObj(in, tx)
    }

    def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit acc: tx.Acc): Elem[T]
  }

  def addType(tpe: Type): Unit      = Impl.addType(tpe)
  def getType(id : Int ): Elem.Type = Impl.getType(id )
}

/** An `Elem` is any type that is globally registered
  * via `Elem.addType` and can be de-serialized through `Elem.read`.
  */
trait Elem[T <: Txn[T]] extends Form[T] with Writable with Disposable[T] with Publisher[T, Any] {
  def tpe: Elem.Type

  /** Selects an event during dispatch. Elements that do not provide events
    * should simply throw an exception. They will in any case not be encountered during dispatch.
    */
  private[lucre] def event(slot: Int): Event[T, Any]

  /** Makes a deep copy of an element, possibly translating it to a different system `Out`. */
  private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out]
}