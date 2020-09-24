/*
 *  BooleanExtensions.scala
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
import de.sciss.lucre.impl.{ExprNodeImpl, ExprTuple1, ExprTuple1Op, ExprTuple2, ExprTuple2Op}
import de.sciss.lucre.{BooleanObj, Copy, Elem, Expr, IntObj, Obj, Pull, Txn}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

object BooleanExtensions  {
  private[this] lazy val _init: Unit = {
    BooleanObj.registerExtension(BooleanTuple1s)
    BooleanObj.registerExtension(BooleanTuple2s)
  }

  def init(): Unit = _init

  private[this] type _Ex[T <: Txn[T]] = BooleanObj[T]

  private[this] object BooleanTuple1s extends Expr.Type.Extension1[BooleanObj] {
    // final val arity = 1
    final val opLo: Int = Not.id
    final val opHi: Int = Not.id

    val name = "Boolean-1 Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      val op: UnaryOp[Boolean, BooleanObj] = opId /* : @switch */ match {
        case Not.id => Not
      }
      val _1 = BooleanObj.read(in)
      new Tuple1[T, Boolean, BooleanObj](targets, op, _1)
    }
  }

  final class Tuple1[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]](
                                                                           protected val targets: Targets[T], val op: ExprTuple1Op[Boolean, T1, BooleanObj, ReprT1], val _1: ReprT1[T])
    extends ExprTuple1[T, Boolean, T1, BooleanObj, ReprT1] with BooleanObj[T] {

    def tpe: Obj.Type = BooleanObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple1[Out, T1, ReprT1](Targets[Out], op, context(_1)).connect()
  }

  private[this] object BooleanTuple2s extends Expr.Type.Extension1[BooleanObj] {
    // final val arity = 2
    final val opLo: Int = And   .id
    final val opHi: Int = IntGeq.id

    val name = "Boolean-2 Ops"

    def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                  (implicit tx: T): _Ex[T] = {
      val op /* : BinaryOp[_, _] */ = (opId: @switch) match {
        case And   .id => And
        case Or    .id => Or
        case Xor   .id => Xor

        case IntEq .id => IntEq
        case IntNeq.id => IntNeq
        case IntLt .id => IntLt
        case IntGt .id => IntGt
        case IntLeq.id => IntLeq
        case IntGeq.id => IntGeq
      }
      op.read(in, targets)
    }
  }

  // ----- operators -----

  final class Ops[T <: Txn[T]](val `this`: _Ex[T]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = _Ex[T]

    // ---- Boolean => Boolean ----

    def unary_!(implicit tx: T): E = Not(a)

    // ---- (Boolean, Boolean) => Boolean ----

    def && (b: E)(implicit tx: T): E = And(a, b)
    def || (b: E)(implicit tx: T): E = Or (a, b)
    def ^  (b: E)(implicit tx: T): E = Xor(a, b)

    // ---- Boolean => Int ----

    def toInt(implicit tx: T): IntObj[T] = IntExtensions.BooleanToInt(a)
  }

  // ----- impl -----

  private[this] abstract class UnaryOp[T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]]
    extends ExprTuple1Op[Boolean, T1, BooleanObj, ReprT1] {

    final def apply[T <: Txn[T]](_1: ReprT1[T])(implicit tx: T): _Ex[T] = _1 match {
      case Expr.Const(a) => BooleanObj.newConst[T](value(a))  // IntelliJ highlight error
      case _ =>
        new Tuple1[T, T1, ReprT1](Targets[T], this, _1).connect()
    }

    def toString[T <: Txn[T]](_1: ReprT1[T]): String = s"$name${_1}"

    def name: String
  }

  final class Tuple2[T <: Txn[T], T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1],
    T2, ReprT2[~ <: Txn[~]] <: Expr[~, T2]](
                                             protected val targets: Targets[T], val op: ExprTuple2Op[Boolean, T1, T2, BooleanObj, ReprT1, ReprT2],
                                             val _1: ReprT1[T], val _2: ReprT2[T])
    extends ExprTuple2[T, Boolean, T1, T2, BooleanObj, ReprT1, ReprT2] with BooleanObj[T] {

    def tpe: Obj.Type = BooleanObj

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Tuple2[Out, T1, ReprT1, T2, ReprT2](Targets[Out], op, context(_1), context(_2)).connect()
  }

  private[this] case object Not extends UnaryOp[Boolean, BooleanObj] {
    final val id = 0
    def value(a: Boolean): Boolean = !a
    def name = "!"
  }

  sealed trait BinaryOp[T1, ReprT1[~ <: Txn[~]] <: Expr[~, T1]]
    extends ExprTuple2Op[Boolean, T1, T1, BooleanObj, ReprT1, ReprT1] {

    def name: String

    def read[T <: Txn[T]](in: DataInput, targets: Targets[T])(implicit tx: T): _Ex[T]

    final def toString[T <: Txn[T]](_1: ReprT1[T], _2: ReprT1[T]): String = s"(${_1} $name ${_2})"
  }

  private[this] trait BooleanBinaryOp extends BinaryOp[Boolean, BooleanObj] { op =>
    def read[T <: Txn[T]](in: DataInput, targets: Targets[T])(implicit tx: T): _Ex[T] = {
      val _1 = BooleanObj.read(in)
      val _2 = BooleanObj.read(in)
      new LazyTuple2[T](targets, op, _1, _2)
    }

    def isLazy: Boolean

    def lazyValue[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T])(implicit tx: T): Boolean

    // def tryValue_1[T <: Txn[T]](_1: Boolean): Option[Boolean]

    final def apply[T <: Txn[T]](a: _Ex[T], b: _Ex[T])(implicit tx: T): _Ex[T] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => BooleanObj.newConst(value(ca, cb))
      case _ => new LazyTuple2[T](Targets[T], this, a, b).connect()
    }
  }

  sealed trait IntBinaryOp extends BinaryOp[Int, IntObj] { op =>
    final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])(implicit tx: T): _Ex[T] = {
      val _1 = IntObj.read(in)
      val _2 = IntObj.read(in)
      new Tuple2[T, Int, IntObj, Int, IntObj](targets, op, _1, _2)
    }

    // ---- impl ----

    final def apply[T <: Txn[T]](a: IntObj[T], b: IntObj[T])(implicit tx: T): _Ex[T] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => BooleanObj.newConst(value(ca, cb))
      case _ => new Tuple2[T, Int, IntObj, Int, IntObj](Targets[T], this, a, b).connect()
    }
  }

  private final class LazyTuple2[T <: Txn[T]](protected val targets: Targets[T], op: BooleanBinaryOp,
                                              _1: _Ex[T], _2: _Ex[T])
    extends ExprNodeImpl[T, Boolean] with BooleanObj[T] {

    def tpe: Obj.Type = BooleanObj

    def connect()(implicit tx: T): this.type = {
      _1.changed ---> changed
      _2.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: T): Unit = {
      _1.changed -/-> changed
      _2.changed -/-> changed
    }

    protected def disposeData()(implicit tx: T): Unit = disconnect()

    def value(implicit tx: T): Boolean = op.lazyValue(_1, _2)

    private[lucre] def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new LazyTuple2(Targets[Out], op, context(_1), context(_2)).connect()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)  // 'node not var'
      // out.writeInt(BooleanObj.typeId)
      out.writeInt(op.id)
      _1.write(out)
      _2.write(out)
    }

    object changed extends Changed {
      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[Boolean]] = {
        val _1c = _1.changed
        val _2c = _2.changed

        val _1ch = if (pull.contains(_1c)) pull(_1c) else None
        // yo crazy mama
        if (!_1ch.exists(_.isSignificant) && op.isLazy) return None

        val _2ch = if (pull.contains(_2c)) pull(_2c) else None

        (_1ch, _2ch) match {
          case (Some(ach), None) =>
            val bv      = _2.value
            val before  = op.value(ach.before, bv)
            val now     = op.value(ach.now, bv)
            if (before == now) None else Some(Change(before, now))
          case (None, Some(bch)) =>
            val av      = _1.value
            val before  = op.value(av, bch.before)
            val now     = op.value(av, bch.now)
            if (before == now) None else Some(Change(before, now))
          case (Some(ach), Some(bch)) =>
            val before  = op.value(ach.before, bch.before)
            val now     = op.value(ach.now, bch.now)
            if (before == now) None else Some(Change(before, now))
          case _ => None
        }
      }
    }

    override def toString: String = op.toString(_1, _2)
  }

  // ---- (Boolean, Boolean) => Boolean ----

  private[this] case object And extends BooleanBinaryOp {
    final val id = 1
    def value(a: Boolean, b: Boolean): Boolean = a && b
    def name = "&&"

    def lazyValue[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T])(implicit tx: T): Boolean =
      _1.value && _2.value

    def isLazy = true

    // def tryValue_1[T <: Txn[T]](_1: Boolean): Option[Boolean] = if (_1) None else Some(false)
  }

  private[this] case object Or extends BooleanBinaryOp {
    final val id = 2
    def value(a: Boolean, b: Boolean): Boolean = a || b
    def name = "||"

    def lazyValue[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T])(implicit tx: T): Boolean =
      _1.value || _2.value

    def isLazy = true

    // def tryValue_1[T <: Txn[T]](_1: Boolean): Option[Boolean] = if (_1) Some(true) else None
  }

  private[this] case object Xor extends BooleanBinaryOp {
    final val id = 3
    def value(a: Boolean, b: Boolean): Boolean = a ^ b
    def name = "^"

    def lazyValue[T <: Txn[T]](_1: _Ex[T], _2: _Ex[T])(implicit tx: T): Boolean =
      _1.value ^ _2.value   // eager actually

    def isLazy = false

    // def tryValue_1[T <: Txn[T]](_1: Boolean): Option[Boolean] = None
  }

  // ---- (Int, Int) => Boolean ----

  case object IntEq extends IntBinaryOp {
    final val id = 4
    def value(a: Int, b: Int): Boolean = a == b
    def name = "sig_=="
  }

  case object IntNeq extends IntBinaryOp {
    final val id = 5
    def value(a: Int, b: Int): Boolean = a != b
    def name = "sig_!="
  }

  case object IntLt extends IntBinaryOp {
    final val id = 6
    def value(a: Int, b: Int): Boolean = a < b
    def name = "<"
  }

  case object IntGt extends IntBinaryOp {
    final val id = 7
    def value(a: Int, b: Int): Boolean = a > b
    def name = ">"
  }

  case object IntLeq extends IntBinaryOp {
    final val id = 8
    def value(a: Int, b: Int): Boolean = a <= b
    def name = "<="
  }

  case object IntGeq extends IntBinaryOp {
    final val id = 9
    def value(a: Int, b: Int): Boolean = a >= b
    def name = ">="
  }
}