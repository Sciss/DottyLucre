/*
 *  Obj.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.impl.ObjImpl
import de.sciss.serial.{DataInput, TFormat}

object Obj {
  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = ObjImpl.read(in)

  /** Copy an object graph with `in` as a leaf.
   * This is short for the following sequence:
   *
   * {{{
   * val c   = Copy[Int, Out]
   * val out = c(in)
   * c.finish()
   * }}}
   */
  def copy[In <: Txn[In], Out <: Txn[Out], Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In])
                                                                        (implicit txIn: In, txOut: Out): Repr[Out] = {
    val context = Copy[In, Out]()
    val res     = context(in)
    context.finish()
    res
  }

  implicit def format[T <: Txn[T]]: TFormat[T, Obj[T]] = ObjImpl.format

  trait Type extends Elem.Type {
    private[this] lazy val _init: Unit = Obj.addType(this)

    override def init(): Unit = {
      super.init()
      _init
    }

    final override def readObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
      val tpe = in.readInt()
      if (tpe !== typeId) sys.error(
        s"Type mismatch, expected $typeId (0x${typeId.toHexString}) but found $tpe (0x${tpe.toHexString})")
      readIdentifiedObj(in)
    }

    override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T]
  }

  def addType(tpe: Type): Unit      = ObjImpl.addType(tpe)
  def getType(id : Int ): Obj.Type  = ObjImpl.getType(id )

  // ---- attributes ----

  type AttrMap    [T <: Txn[T]]             = TMap.Modifiable[T, String, Obj]

//  type AttrUpdate [T <: Txn[T]]             = evt.Map.Update [T, String, Obj]
//  val  AttrAdded    : evt.Map.Added.type    = evt.Map.Added
//  type AttrAdded  [T <: Txn[T]]             = evt.Map.Added  [T, String, Obj[T]]
//  val  AttrRemoved  : evt.Map.Removed.type  = evt.Map.Removed
//  type AttrRemoved[T <: Txn[T]]             = evt.Map.Removed[T, String, Obj[T]]
//  val  AttrReplaced : evt.Map.Replaced.type = evt.Map.Replaced
//  type AttrReplaced[T <: Txn[T]]            = evt.Map.Replaced[T, String, Obj[T]]
//
  /* implicit */ def attrMapFormat[T <: Txn[T]]: TFormat[T, AttrMap[T]] =
    anyAttrMapFmt.asInstanceOf[TFormat[T, AttrMap[T]]]

  private[this] val anyAttrMapFmt = TMap.Modifiable.format[AnyTxn, String, Obj]

  final val attrName = "name"
}

/** An `Obj` is a type of element that has an `S#Id` identifier and
 * an attribute map. It can be the origin of event dispatch.
 */
trait Obj[T <: Txn[T]] extends Elem[T] with Mutable[/*Ident[T],*/ T] {
  override def toString = s"Obj$id"

  override def tpe: Obj.Type

  final def attr(implicit tx: T): Obj.AttrMap[T] = tx.attrMap(this)
}