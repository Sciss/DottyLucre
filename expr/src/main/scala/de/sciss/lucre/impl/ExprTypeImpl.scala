/*
 *  ExprTypeImpl.scala
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

package de.sciss.lucre
package impl

import de.sciss.lucre
import de.sciss.lucre.Event.Targets
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

import scala.annotation.switch
import scala.language.implicitConversions

trait ExprTypeImpl[A1, Repr[~ <: Txn[~]] <: Expr[~, A1]] extends Expr.Type[A1, Repr] with ExprTypeExtensibleImpl1[Repr] { self =>
  // ---- public ----

  implicit final def tpe: Expr.Type[A1, Repr] = this

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): E[T] =
    (in.readByte(): @switch) match {
      case 3 => readIdentifiedConst[T](in)
      case 0 =>
        val targets = Event.Targets.readIdentified[T](in)
        in.readByte() match {
          case 0 => readIdentifiedVar[T](in, targets)
          case 1 => readNode(in, targets)
        }
      case cookie => readCookie(in, cookie)
    }

  /** The default implementation reads a type `Int` as operator id `Int`
   * which will be resolved using `readOpExtension`.
   */
  protected def readNode[T <: Txn[T]](in: DataInput, targets: Event.Targets[T])
                                     (implicit tx: T): E[T] = {
    val opId = in.readInt()
    readExtension(op = opId, in = in, targets = targets)
  }

  /** Reads an identified object whose cookie is neither `3` (constant) nor `0` (node).
   * By default this throws an exception. Sub-classes may use a cookie greater
   * than `3` for other constant types.
   */
  protected def readCookie[T <: Txn[T]](in: DataInput, cookie: Byte)(implicit tx: T): E[T] =  // sub-class may need tx
    sys.error(s"Unexpected cookie $cookie")

  implicit final def format[T <: Txn[T]]: TFormat[T, E[T]] /* EventLikeFormat[S, Repr[T]] */ =
    anyFmt.asInstanceOf[Fmt[T]]

  implicit final def varFormat[T <: Txn[T]]: TFormat[T, Var[T]] /* Format[T, S#Acc, ReprVar[T]] */ =
    anyVarFmt.asInstanceOf[VarFmt[T]]

  // repeat `implicit` here because IntelliJ IDEA will not recognise it otherwise (SCL-9076)
  implicit final def newConst[T <: Txn[T]](value: A)(implicit tx: T): Const[T] =
    mkConst[T](tx.newId(), value)

  final def newVar[T <: Txn[T]](init: E[T])(implicit tx: T): Var[T] = {
    val targets = Event.Targets[T]()
    val ref     = targets.id.newVar[E[T]](init)
    mkVar[T](targets, ref, connect = true)
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T]
  protected def mkVar  [T <: Txn[T]](targets: Event.Targets[T], vr: lucre.Var[T, E[T]], connect: Boolean)(implicit tx: T): Var[T]

  override final def read[T <: Txn[T]](in: DataInput)(implicit tx: T): E[T] =
    format[T].readT(in)

  override final def readConst[T <: Txn[T]](in: DataInput)(implicit tx: T): Const[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedConst(in)
  }

  @inline
  private[this] def readIdentifiedConst[T <: Txn[T]](in: DataInput)(implicit tx: T): Const[T] = {
    val id      = tx.readId(in)
    val value   = valueFormat.read(in)
    mkConst[T](id, value)(tx)
  }

  override final def readVar[T <: Txn[T]](in: DataInput)(implicit tx: T): Var[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val targets = Event.Targets.read[T](in)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedVar(in, targets)
  }

  @inline
  private[this] def readIdentifiedVar[T <: Txn[T]](in: DataInput, targets: Event.Targets[T])
                                                  (implicit tx: T): Var[T] = {
    val ref = targets.id.readVar[E[T]](in)
    mkVar[T](targets, ref, connect = false)(tx)
  }

  // ---- private ----

  protected trait ConstImpl[T <: Txn[T]] // (val id: S#Id, val constValue: A)
    extends ExprConstImpl[T, A] {

    final def tpe: Obj.Type = self

    final protected def writeData(out: DataOutput): Unit = valueFormat.write(constValue, out)

    private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      mkConst[Out](txOut.newId(), constValue)
  }

  protected trait VarImpl[T <: Txn[T]]
    extends ExprVarImpl[T, A, E[T]] {

    final def tpe: Obj.Type = self

    private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      val newTgt = Event.Targets[Out]()
      val newVr  = newTgt.id.newVar(context(ref()))
      mkVar[Out](newTgt, newVr, connect = true)
    }
  }

  private[this] val anyFmt    = new Fmt   [AnyTxn]
  private[this] val anyVarFmt = new VarFmt[AnyTxn]

  private[this] final class VarFmt[T <: Txn[T]] extends WritableFormat[T, Var[T]] {
    override def readT(in: DataInput)(implicit tx: T): Var[T] = readVar[T](in)
  }

  private[this] final class Fmt[T <: Txn[T]] extends WritableFormat[T, E[T]] {
    override def readT(in: DataInput)(implicit tx: T): E[T] = {
      val tpe = in.readInt()
      if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
      readIdentifiedObj(in)
    }
  }
}

trait ExprTypeExtensible[Ext >: Null <: Expr.Type.Extension] {
  protected def mkExtArray(size: Int): Array[Ext]

  private[this] var extensions = mkExtArray(0) // new Array[Ext](0)

  final protected def addExtension(extensions: Array[Ext], ext: Ext): Array[Ext] = {
    val opLo = ext.opLo
    val opHi = ext.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val idx0  = extensions.indexWhere(_.opLo > opHi)
    val idx   = if (idx0 < 0) extensions.length else idx0
    if (idx > 0) {
      val pred = extensions(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $ext")
    }
    val len   = extensions.length
    val extensions1 = mkExtArray(len + 1) // new Array[Ext](len + 1)
    System.arraycopy(extensions, 0, extensions1, 0, len)
    extensions1(len) = ext
    extensions1
  }

  final protected def findExt(extensions: Array[Ext], op: Int): Ext = {
    var index = 0
    var low   = 0
    var high  = extensions.length - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = extensions(index)
      if (ext.opLo <= op) {
        if (ext.opHi >= op) return ext
        low = index + 1
      } else {
        high = index - 1
      }
    }
    null
  }

  final def registerExtension(ext: Ext): Unit = extensions = addExtension(extensions, ext)

  final protected def findExt(op: Int): Ext = findExt(extensions, op)
}

trait ExprTypeExtensibleImpl1[Repr[~ <: Txn[~]]] extends ExprTypeExtensible[Expr.Type.Extension1[Repr]] {
  protected def mkExtArray(size: Int): Array[Expr.Type.Extension1[Repr]] = new Array(size)

  final protected def readExtension[T <: Txn[T]](op: Int, in: DataInput, targets: Targets[T])
                                                (implicit tx: T): Repr[T] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[T](op, in, targets)
  }
}