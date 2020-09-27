/*
 *  Attr.scala
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

import de.sciss.lucre.expr.graph.impl.{ExpandedAttrSet, ExpandedAttrUpdate, StmObjAttrMapCellView, StmObjCtxCellView}
import de.sciss.lucre.expr.impl.CellViewImpl.CatVarImpl
import de.sciss.lucre.expr.{CellView, Context, IAction, IControl}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, Disposable, Form, IChangeEvent, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn, Obj => LObj}
import de.sciss.model.Change

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object Attr {
  trait Like[A] {
    def update(in: Ex[A]): Control
    def set   (in: Ex[A]): Act
  }

  private[lucre] def resolveNestedIn[T <: Txn[T], A](objOpt: Option[LObj[T]], key: String)
                                                    (implicit tx: T,
                                                     bridge: Obj.Bridge[A]): Option[CellView.Var[T, Option[A]]] = {
    @tailrec
    def loop(prev: Option[LObj[T]], sub: String): Option[CellView.Var[T, Option[A]]] =
      prev match {
        case Some(obj) =>
          val i = sub.indexOf(':')
          if (i < 0) {
            val attrView = bridge.cellView[T](obj, sub)
            Some(attrView)
          } else {
            val head = sub.substring(0, i)
            val tail = sub.substring(i + 1)
            val next = obj.attr.get(head)
            loop(next, tail)
          }

        case _ => None
      }

    loop(objOpt, key)
  }

  private[lucre] def resolveNested[T <: Txn[T], A](key: String)(implicit ctx: Context[T], tx: T,
                                                                bridge: Obj.Bridge[A]): CellView[T, Option[A]] = {
    val isNested = key.contains(":")

    if (isNested) {
      val head :: firstSub :: tail = key.split(":").toList

      @tailrec
      def loop(parent: CellView[T, Option[LObj[T]]], sub: String, rem: List[String]): CellView[T, Option[A]] =
        rem match {
          case Nil =>
            parent.flatMapTx { implicit tx => child => bridge.cellValue(child, sub) }

          case next :: tail =>
            val childView = parent.flatMapTx { implicit tx => child => child.attr.get(key) }
            loop(childView, next, tail)
        }

      val ctxHead   = new StmObjCtxCellView[T](ctx.attr, head)
      val ctxFull   = loop(ctxHead, firstSub, tail)
      ctx.selfOption match {
        case Some(self) =>
          val objHead   = new StmObjAttrMapCellView[T](self.attr, head, tx)
          val objFull   = loop(objHead, firstSub, tail)
          ctxFull orElse objFull
        case None =>
          ctxFull
      }

    } else {
      val ctxFull = bridge.contextCellView(key)
      ctx.selfOption match {
        case Some(self) =>
          val objFull = bridge.cellView(self, key)
          ctxFull orElse objFull
        case None =>
          ctxFull
      }
    }
  }

  // similar to `CellViewImpl.OptionOrElseImpl` but adding `.Var` support
  private final class NestedVarCellView[T <: Txn[T], A](firstP  : CellView[T, Option[LObj[T]]],
                                                        secondP : CellView[T, Option[LObj[T]]],
                                                        lastSub : String)(implicit bridge: Obj.Bridge[A])
    extends CellView.Var[T, Option[A]] {

    def apply()(implicit tx: T): Option[A] = {
      val parOpt = firstP() orElse secondP()
      parOpt match {
        case Some(par)  => bridge.cellValue(par, lastSub)
        case None       => None
      }
    }

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
      val f: T => Option[LObj[T]] => Unit = { implicit tx => _ => fun(tx)(apply()) }
      val r1 = firstP .react(f)
      val r2 = secondP.react(f)
      Disposable.seq(r1, r2)
    }

    def update(v: Option[A])(implicit tx: T): Unit = {
      val parOpt = firstP() orElse secondP()
      parOpt match {
        case Some(par)  => bridge.cellView(par, lastSub).update(v)
        case None       =>
      }
    }
  }

  // XXX TODO --- all pretty hack'ish
  private final class FlatVarCellView[T <: Txn[T], A, B](firstP   : CellView[T, Option[A]],
                                                         firstVr  : Option[Var.Expanded[T, B]],
                                                         secondP  : CellView.Var[T, Option[A]],
                                                         )(implicit bridge: Obj.Bridge[A])
    extends CellView.Var[T, Option[A]] {

    def apply()(implicit tx: T): Option[A] =
      firstP() orElse secondP()

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] = {
      val f: T => Option[A] => Unit = { implicit tx => _ => fun(tx)(apply()) }
      val r1 = firstP .react(f)
      val r2 = secondP.react(f)
      Disposable.seq(r1, r2)
    }

    def update(v: Option[A])(implicit tx: T): Unit = {
      firstVr match {
        case Some(vr) =>
          vr.fromAny.fromAny(v.get).foreach { vT =>
            vr.update(new Const.Expanded(vT))
          }
        case None =>
          secondP.update(v)
      }
    }
  }

  private[lucre] def resolveNestedVar[T <: Txn[T], A](key: String)(implicit ctx: Context[T], tx: T,
                                                                   bridge: Obj.Bridge[A]): CellView.Var[T, Option[A]] = {
    val isNested = key.contains(":")

    if (isNested) {
      val head :: firstSub :: tail = key.split(":").toList

      @tailrec
      def loop(parent: CellView[T, Option[LObj[T]]], sub: String, rem: List[String]): (CellView[T, Option[LObj[T]]], String) =
        rem match {
          case Nil =>
            // new CatVarImpl[T, LObj[T], A](parent, child => bridge.cellView(child, sub))
            (parent, sub)

          case next :: tail =>
            val childView = parent.flatMapTx { implicit tx =>  child =>
              child.attr.get(key)
            }
            loop(childView, next, tail)
        }

      val ctxHead             = new StmObjCtxCellView[T](ctx.attr, head)
      val (ctxFullP, lastSub) = loop(ctxHead, firstSub, tail)
      ctx.selfOption match {
        case Some(self) =>
          val objHead       = new StmObjAttrMapCellView[T](self.attr, head, tx)
          val (objFullP, _) = loop(objHead, firstSub, tail)
          new NestedVarCellView(ctxFullP, objFullP, lastSub)

        case None =>
          new CatVarImpl[T, LObj[T], A](ctxFullP)({ implicit tx => child =>
            bridge.cellView(child, lastSub)
          })
      }

    } else {
      ctx.selfOption match {
        case Some(self) =>
          val firstP  = bridge.contextCellView(key)
          val secondP = bridge.cellView(self, key)
          val opt: Option[Form[T]] = ctx.attr.get(key)
          val firstVr = opt match {
            case Some(ex: Var.Expanded[T, _]) => Some(ex)
            case _                            => None
          }
          new FlatVarCellView(firstP, firstVr, secondP)

        case None =>  // if there is no 'self', simply give up on the idea of attributes
          CellView.Var.empty
      }
    }
  }

  object WithDefault {
    def apply[A](key: String, default: Ex[A])(implicit bridge: Obj.Bridge[A]): WithDefault[A] =
      Impl(key, default)

    private final case class Impl[A](key: String, default: Ex[A])(implicit val bridge: Obj.Bridge[A])
      extends WithDefault[A] with ProductWithAdjuncts {

      type Repr[T <: Txn[T]] = IExpr[T, A]

      override def productPrefix: String = s"Attr$$WithDefault" // serialization

      def update(in: Ex[A]): Control  = Attr.Update(in, key)
      def set   (in: Ex[A]): Act      = Attr.Set   (in, key)

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val defaultEx: Repr[T] = default.expand[T]
        import ctx.targets
        val attrView = resolveNested(key)
        new WithDefault.Expanded[T, A](key, attrView, defaultEx, tx)
      }

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }

    private[lucre] final class Expanded[T <: Txn[T], A](key: String, attrView: CellView[T, Option[A]], default: IExpr[T, A],
                                                        tx0: T)
                                                       (implicit protected val targets: ITargets[T])
      extends IExpr[T, A] with IChangeGeneratorEvent[T, A] {

      override def toString: String = s"Attr.WithDefault($key, $default)"

      private[this] val ref = Ref(attrView()(tx0))

      private[this] val obsAttr = attrView.react { implicit tx => now =>
        val before  = ref.swap(now)(tx.peer)
        if (before != now) {
          val before1   = before.getOrElse(default.value)
          val now1      = now   .getOrElse(default.value)
          val ch        = Change(before1, now1)
          if (ch.isSignificant) fire(ch)
        }
      } (tx0)

      default.changed.--->(this)(tx0)

      def value(implicit tx: T): A = {
        val opt = attrView()
        opt.getOrElse(default.value)
      }

      private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
        val dch = default.changed
        if (pull.contains(dch) && ref.get(tx.peer).isEmpty) {
          pull.applyChange(dch)
        } else if (pull.isOrigin(this)) {
          pull.resolveExpr(this)
        } else {
          value
        }
      }

      def dispose()(implicit tx: T): Unit = {
        default.changed -/-> this
        obsAttr.dispose()
      }

      def changed: IChangeEvent[T, A] = this
    }
  }
  // N.B. we use a trait here not a case class, because
  // we reuse the interface elsewhere (SP -> Artifact)
  trait WithDefault[A] extends Ex[A] with Like[A] {
    def default: Ex[A]
  }

  private[lucre] final class Expanded[T <: Txn[T], A](key: String, attrView: CellView[T, Option[A]], tx0: T)
                                                     (implicit protected val targets: ITargets[T])
    extends IExpr[T, Option[A]] with IChangeGeneratorEvent[T, Option[A]] {

    override def toString: String = s"Attr($key)"

    // println("Attr.Expanded - created")

    private[this] val ref = Ref(value(tx0))

    private[this] val obsAttr = attrView.react { implicit tx => now =>
      val before = ref.swap(now)(tx.peer)
      val ch = Change(before, now)
      // println(s"Attr.Expanded change $ch")
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Option[A] = attrView()

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Option[A] =
      pull.resolveExpr(this)

    def changed: IChangeEvent[T, Option[A]] = this

    def dispose()(implicit tx: T): Unit = {
      // println("Attr.Expanded - dispose")
      obsAttr.dispose()
    }
  }

  final case class Update[A](source: Ex[A], key: String)(implicit bridge: Obj.Bridge[A])
    extends Control with ProductWithAdjuncts {

    override def productPrefix: String = s"Attr$$Update"  // serialization

    type Repr[T <: Txn[T]] = IControl[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val attrView  = resolveNestedVar(key)
      val peer      = new ExpandedAttrUpdate[T, A](source.expand[T], attrView, tx)
      IControl.wrap(peer)
    }

    override def adjuncts: scala.List[Adjunct] = bridge :: Nil
  }

  final case class Set[A](source: Ex[A], key: String)(implicit bridge: Obj.Bridge[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Attr$$Set"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val attrView = resolveNestedVar(key)
      new ExpandedAttrSet[T, A](attrView, source.expand[T], tx)
    }

    override def adjuncts: scala.List[Adjunct] = bridge :: Nil
  }
}
final case class Attr[A](key: String)(implicit val bridge: Obj.Bridge[A])
  extends Ex[Option[A]] with Attr.Like[A] with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

  def update(in: Ex[A]): Control  = Attr.Update(in, key)
  def set   (in: Ex[A]): Act      = Attr.Set   (in, key)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val attrView = Attr.resolveNested(key)
    new Attr.Expanded[T, A](key, attrView, tx)
  }

  override def adjuncts: scala.List[Adjunct] = bridge :: Nil
}
