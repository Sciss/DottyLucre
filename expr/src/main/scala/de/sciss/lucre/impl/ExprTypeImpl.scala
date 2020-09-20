/*
 *  ExprTypeImpl.scala
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
package impl

import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.lucre

import scala.annotation.switch
import scala.language.implicitConversions

trait ExprTypeImpl[A1, Repr[~ <: Txn[~]] <: Expr[~, A1]] extends Expr.Type[A1, Repr] /*with TypeImpl1[Repr]*/ { self =>
  // ---- public ----

//  implicit final def tpe: Type.Expr[A1, Repr] = this

  def readIdentifiedObj[T <: Txn[T]](in: DataInput, tx: T)(implicit access: tx.Acc): E[T] =
    (in.readByte(): @switch) match {
      case 3 => readIdentifiedConst[T](in, tx)
      case 0 =>
        val targets = Event.Targets.readIdentified[T](in, tx)
        in.readByte() match {
          case 0 => readIdentifiedVar[T](in, tx, targets)
          case 1 => readNode(in, tx, targets)
        }
      case cookie => readCookie(in, tx, cookie)
    }

  /** The default implementation reads a type `Int` as operator id `Int`
   * which will be resolved using `readOpExtension`.
   */
  protected def readNode[T <: Txn[T]](in: DataInput, tx: T, targets: Event.Targets[T])
                                     (implicit access: tx.Acc): E[T] = {
    val opId = in.readInt()
    ??? // readExtension(op = opId, in = in, access = access, targets = targets)
  }

  /** Reads an identified object whose cookie is neither `3` (constant) nor `0` (node).
   * By default this throws an exception. Sub-classes may use a cookie greater
   * than `3` for other constant types.
   */
  protected def readCookie[T <: Txn[T]](in: DataInput, tx: T, cookie: Byte)(implicit access: tx.Acc): E[T] =  // sub-class may need tx
    sys.error(s"Unexpected cookie $cookie")

  implicit final def serializer[T <: Txn[T]]: TSerializer[T, E[T]] /* EventLikeSerializer[S, Repr[T]] */ =
    anySer.asInstanceOf[Ser[T]]

  implicit final def varSerializer[T <: Txn[T]]: TSerializer[T, Var[T]] /* Serializer[T, S#Acc, ReprVar[T]] */ =
    anyVarSer.asInstanceOf[VarSer[T]]

  // repeat `implicit` here because IntelliJ IDEA will not recognise it otherwise (SCL-9076)
  implicit final def newConst[T <: Txn[T]](value: A)(implicit tx: T): Const[T] =
    mkConst[T](tx.newId(), value)

  final def newVar[T <: Txn[T]](init: E[T])(implicit tx: T): Var[T] = {
    val targets = Event.Targets[T]()
    val ref     = targets.id.newVar[E[T]](init)
    mkVar[T](targets, ref, connect = true)
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T]
  protected def mkVar  [T <: Txn[T]](targets: Event.Targets[T], vr: lucre.Var[E[T]], connect: Boolean)(implicit tx: T): Var[T]

  final def read[T <: Txn[T]](in: DataInput, tx: T)(implicit access: tx.Acc): E[T] =
    serializer[T].read(in, tx)

  final def readConst[T <: Txn[T]](in: DataInput, tx: T)(implicit access: tx.Acc): Const[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val cookie = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedConst(in, tx)
  }

  @inline
  private[this] def readIdentifiedConst[T <: Txn[T]](in: DataInput, tx: T)(implicit access: tx.Acc): Const[T] = {
    val id      = tx.readId(in)
    val value   = valueSerializer.read(in)
    mkConst[T](id, value)(tx)
  }

  final def readVar[T <: Txn[T]](in: DataInput, tx: T)(implicit access: tx.Acc): Var[T] = {
    val tpe = in.readInt()
    if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
    val targets = Event.Targets.read[T](in, tx)
    val cookie = in.readByte()
    if (cookie != 0) sys.error(s"Unexpected cookie $cookie")
    readIdentifiedVar(in, tx, targets)
  }

  @inline
  private[this] def readIdentifiedVar[T <: Txn[T]](in: DataInput, tx: T, targets: Event.Targets[T])
                                                  (implicit access: tx.Acc): Var[T] = {
    val ref = targets.id.readVar[E[T]](in)
    mkVar[T](targets, ref, connect = false)(tx)
  }

  // ---- private ----

  protected trait ConstImpl[T <: Txn[T]] // (val id: S#Id, val constValue: A)
    extends ExprConstImpl[T, A] {

    final def tpe: Obj.Type = self

    final protected def writeData(out: DataOutput): Unit = valueSerializer.write(constValue, out)

    private[lucre] def copy[Out <: Txn[Out]]()(implicit txOut: Out, context: Copy[T, Out]): Elem[Out] =
      mkConst[Out](txOut.newId(), constValue)
  }

  protected trait VarImpl[T <: Txn[T]]
    extends ExprVarImpl[T, A, E[T]] {

    final def tpe: Obj.Type = self

    private[lucre] def copy[Out <: Txn[Out]]()(implicit txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      val newTgt = Event.Targets[Out]()
      val newVr  = newTgt.id.newVar(context(ref()))
      mkVar[Out](newTgt, newVr, connect = true)
    }
  }

  private[this] val anySer    = new Ser   [AnyTxn]
  private[this] val anyVarSer = new VarSer[AnyTxn]

  private[this] final class VarSer[T <: Txn[T]] extends TSerializer[T, Var[T]] /* with Reader[S, Var[T]] */ {
    def write(v: Var[T], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, tx: T)(implicit access: tx.Acc): Var[T] = readVar[T](in, tx)
  }

  private[this] final class Ser[T <: Txn[T]] extends TSerializer[T, E[T]] /* EventLikeSerializer[S, Ex[T]] */ {
    def write(ex: E[T], out: DataOutput): Unit = ex.write(out)

    def read(in: DataInput, tx: T)(implicit access: tx.Acc): E[T] = {
      val tpe = in.readInt()
      if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
      readIdentifiedObj(in, tx)
    }
  }
}