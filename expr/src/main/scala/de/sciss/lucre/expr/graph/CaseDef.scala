/*
 *  CaseDef.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.{FromAny, HasDefault}
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IControl, graph}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, Disposable, IChangeEvent, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object CaseDef {
  sealed trait Expanded[T <: Txn[T], A] extends IExpr[T, A] {
    def fromAny: FromAny[A]

    def select(value: Any)(implicit tx: T): Boolean

    def commit()(implicit tx: T): Unit
  }
}
sealed trait CaseDef[A] extends Ex[A] with ProductWithAdjuncts {
  type Repr[T <: Txn[T]] <: CaseDef.Expanded[T, A]

  def fromAny: FromAny[A]

  def adjuncts: List[Adjunct] = fromAny :: Nil
}

object Quote {
  private final class ExpandedImpl[T <: Txn[T], A](in: IExpr[T, A])(implicit val fromAny: FromAny[A])
    extends Expanded[T, A] {

    def select(value: Any)(implicit tx: T): Boolean =
      fromAny.fromAny(value) match {
        case Some(v) if v == in.value  => true
        case _                         => false
      }

    def commit()(implicit tx: T): Unit = ()

    def value(implicit tx: T): A = in.value

    def dispose()(implicit tx: T): Unit = in.dispose()

    def changed: IChangeEvent[T, A] = in.changed
  }

  trait Expanded[T <: Txn[T], A] extends CaseDef.Expanded[T, A]
}
final case class Quote[A](in: Ex[A])(implicit val fromAny: FromAny[A])
  extends CaseDef[A] {

  type Repr[T <: Txn[T]] = CaseDef.Expanded[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new Quote.ExpandedImpl(in.expand[T])
}

object Var {
  def apply[A](init: Ex[A])(implicit from: FromAny[A]): Var[A] = Impl(init)

  def apply[A]()(implicit from: FromAny[A], default: HasDefault[A]): Var[A] =
    Impl(Const(default.defaultValue))

//  implicit final class Ops[A](private val x: Var[A]) extends AnyVal {
//    def set(in: Ex[A]): Act = Set(x, in)
//  }

  final case class Set[A](vr: Var[A], in: Ex[A]) extends Act {
    override def productPrefix: String = s"Var$$Set"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new SetExpanded(vr.expand[T], in.expand[T])
  }

  final case class Update[A](vr: Var[A], in: Ex[A]) extends Control {
    override def productPrefix: String = s"Var$$Update"  // serialization

    type Repr[T <: Txn[T]] = IControl[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val peer = new UpdateExpanded(vr.expand[T], in.expand[T], tx)
      IControl.wrap(peer)
    }
  }

  trait Expanded[T <: Txn[T], A] extends CaseDef.Expanded[T, A] with IExpr.Var[T, A]

  // ---- private ----

  private final class SetExpanded[T <: Txn[T], A](vr: Var.Expanded[T, A], in: IExpr[T, A])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      // If the 'set' action is interpreted correctly, it should
      // mean that we latch the value; if otherwise we would just
      // use `vr.update(in)`, then we could as well just
      // refer to `in` directly and the operation would not
      // make much sense. And so `Var.Set` works like `ExpandedAttrSet`.
      val v = in.value
      vr.update(new Const.Expanded(v))
    }
  }

  private final class UpdateExpanded[T <: Txn[T], A](vr: Var.Expanded[T, A], source: IExpr[T, A], tx0: T)
    extends Disposable[T] {

    private[this] val obs = source.changed.react { implicit tx => upd =>
      vr.update(new Const.Expanded(upd.now))
    } (tx0)

    def dispose()(implicit tx: T): Unit =
      obs.dispose()
  }

  private final class ExpandedImpl[T <: Txn[T], A](init: IExpr[T, A], tx0: T)
                                                  (implicit protected val targets: ITargets[T],
                                                   val fromAny: FromAny[A])
    extends Expanded[T, A] with IChangeGeneratorEvent[T, A] {

    private[this] val ref     = Ref(init)
    private[this] val selRef  = Ref.make[A]()

    init.changed.--->(changed)(tx0)

    def apply()(implicit tx: T): IExpr[T, A] = ref()

    def swap(value: IExpr[T, A])(implicit tx: T): IExpr[T, A] = {
      val old = apply()
      update(value)
      old
    }

    def value(implicit tx: T): A = ref().value

    def update(v: IExpr[T, A])(implicit tx: T): Unit = {
      val before = ref()
      if (before != v) {
        before.changed -/-> this.changed
        ref() = v
        v     .changed ---> this.changed

        val beforeV = before.value
        val exprV   = v     .value
        fire(Change(beforeV, exprV))
      }
    }

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
      if (pull.isOrigin(this)) pull.resolveExpr(this)
      else pull.expr(ref())

    def select(value: Any)(implicit tx: T): Boolean =
      fromAny.fromAny(value) match {
        case Some(v) =>
          selRef() = v
          true
        case _ =>
          false
      }

//    def commit()(implicit tx: T): Unit =
//      ref() = new graph.Const.Expanded(selRef())

    def commit()(implicit tx: T): Unit =
      update(new graph.Const.Expanded(selRef()))

    def dispose()(implicit tx: T): Unit =
      ref().changed -/-> changed

    def changed: IChangeEvent[T, A] = this
  }

  private final case class Impl[A](init: Ex[A])(implicit val fromAny: FromAny[A]) extends Var[A] {
    override def productPrefix: String = "Var"  // serialization

    def update(in: Ex[A]): Control = Update(this, in)

    def set(in: Ex[A]): Act = Set(this, in)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new Var.ExpandedImpl[T, A](init.expand[T], tx)
    }
  }
}
trait Var[A] extends Ex[A] with CaseDef[A] with Attr.Like[A] with ProductWithAdjuncts {
  type Repr[T <: Txn[T]] = Var.Expanded[T, A]
}
