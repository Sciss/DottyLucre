/*
 *  StringExtensions.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{ExprTuple2, ExprTuple2Op}
import de.sciss.lucre.{Copy, Elem, Expr, Obj, StringObj, Txn}
import de.sciss.serial.DataInput

object StringExtensions  {
  private[this] lazy val _init: Unit = {
    StringObj.registerExtension(StringTuple2s)
  }

  def init(): Unit = _init

  type _Ex[T <: Txn[T]] = StringObj[T]

  private[this] object StringTuple2s extends Expr.Type.Extension1[StringObj] {
    // final val arity = 2
    final val opLo: Int = BinaryOp.Append.id
    final val opHi: Int = BinaryOp.Append.id

    val name = "String-2 Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      import BinaryOp._
      val op: Op = opId /* : @switch */ match {
        case Append.id => Append
      }
      val _1 = StringObj.read(in)
      val _2 = StringObj.read(in)
      new Tuple2[T, String, StringObj, String, StringObj](targets, op, _1, _2)
    }
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[String, T1, T2, StringObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, String, T1, T2, StringObj, ReprT1, ReprT2] with StringObj[T] {

    def tpe: Obj.Type = StringObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out](), op, context(_1), context(_2)).connect()
  }

  // ----- operators -----

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = _Ex[T]

    import BinaryOp._

    def ++(b: E)(implicit tx: T): E = Append(ex, b)
  }

  private object BinaryOp {
    sealed abstract class Op extends ExprTuple2Op[String, String, String, StringObj, StringObj, StringObj] {
      def id: Int
      final def apply[T <: Txn[T]](a: _Ex[T], b: _Ex[T])(implicit tx: T): _Ex[T] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => StringObj.newConst(value(ca, cb))
        case _  =>
          new Tuple2[T, String, StringObj, String, StringObj](Targets[T](), this, a, b).connect()
      }

      def value(a: String, b: String): String

      def toString[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.indexOf('$') + 1
        s"${cn.charAt(i).toLower}${cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)}"
      }
    }

    case object Append extends Op {
      final val id = 0
      def value(a: String, b: String): String = a + b
    }
  }
}