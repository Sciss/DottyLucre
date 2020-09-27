/*
 *  CellViewImpl.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.Obj.AttrMap
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.{EditAttrMap, EditExprVar}
import de.sciss.lucre.expr.CellView
import de.sciss.lucre.{Disposable, Form, MapObj, MapObjLike, Obj, Source, Txn, Expr => _Expr, ExprLike => _ExprLike, Var => LVar}
import de.sciss.model.Change
import de.sciss.serial.TFormat

import scala.concurrent.stm.Ref

object CellViewImpl {
  def const[T <: Txn[T], A](value: A): CellView[T, A] = new Const(value)

  def expr[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](x: _Ex[T])(implicit tx: T,
                                                                       tpe: _Expr.Type[A, _Ex]): CellView[T, A] = {
    import tpe.{format, varFormat}
    x match {
      case tpe.Var(vr) =>
        new ExprVar[T, A, _Ex](tx.newHandle(vr))

      case _ =>
        new Expr[T, A, _Ex](tx.newHandle(x))
    }
  }

  def exprMap[T <: Txn[T], K, A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](map: MapObj[T, K, _Ex], key: K)
                                                                 (implicit tx: T, tpe: _Expr.Type[A, _Ex], 
                                                                  keyType: MapObj.Key[K])
  : CellView[T, Option[A]] = {
    // import tpe.format
    map.modifiableOption.fold[CellView[T, Option[A]]] {
      new ExprMap[T, K, A, _Ex  /* , Change[A] */](tx.newHandle(map), key /* , ch =>
        if (ch.isSignificant) Some(ch.now) else None */)
    } { mv =>
      new ExprModMap[T, K, A, _Ex](tx.newHandle(mv), key)
    }
  }

  def attr[T <: Txn[T], A, E[~ <: Txn[~]] <: _Expr[~, A]](map: Obj.AttrMap[T], key: String)
                                                         (implicit tx: T,
                                                          tpe: _Expr.Type[A, E]): CellView.Var[T, Option[A]] =
    new PlainAttrImpl[T, A, E](tx.newHandle(map), key)

  /** Additionally uses undo manager when present. */
  def attrUndoOpt[T <: Txn[T], A, E[~ <: Txn[~]] <: _Expr[~, A]](map: Obj.AttrMap[T], key: String)
                                                                (implicit tx: T,
                                                                 tpe: _Expr.Type[A, E]): CellView.Var[T, Option[A]] =
    new UndoAttrImpl[T, A, E](tx.newHandle(map), key)

  def exprLike[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](x: _Ex[T])
                                                               (implicit tx: T,
                                                                format: TFormat[T, _Ex[T]]): CellView[T, A] =
    new Expr[T, A, _Ex](tx.newHandle(x))

  // ---- impl ----

  private[lucre] trait ExprMapObjLike[T <: Txn[T], K, A, _Ex[~ <: Txn[~]] <: _Expr[~, A] /* , U */]
    extends CellView[T, Option[A]] {

    protected def h: Source[T, MapObj[T, K, _Ex]]
    protected val key: K

    type Repr = Option[_Ex[T]]

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] =
      new ExprMapObjLikeObs(h(), key, fun, tx)

    protected def repr(implicit tx: T): Repr

    def apply()(implicit tx: T): Option[A] = repr.map(_.value)
  }

  private final class ExprMapObjLikeObs[T <: Txn[T], K, A, _Ex[~ <: Txn[~]] <: _Expr[~, A], 
    U](map: MapObj[T, K, _Ex], key: K, fun: T => Option[A] => Unit, tx0: T)
    extends Disposable[T] {

    private val valObs = Ref(null: Disposable[T])

    private val mapObs = map.changed.react { implicit tx => u =>
      u.changes.foreach {
        case MapObj.Added  (`key`, expr) =>
          valueAdded(expr)
          // XXX TODO -- if we moved this into `valueAdded`, the contract
          // could be that initially the view is updated
          val now0 = expr.value
          fun(tx)(Some(now0))
        case MapObj.Removed(`key`, _ ) =>
          if (valueRemoved()) fun(tx)(None)
        case _ =>
      }
    } (tx0)

    map.get(key)(tx0).foreach(valueAdded(_)(tx0))

    private def valueAdded(expr: _Ex[T])(implicit tx: T): Unit = {
      val res = expr.changed.react { implicit tx => {
        case Change(_, now) =>
          fun(tx)(Some(now))
        //            val opt = mapUpdate(ch)
        //            if (opt.isDefined) fun(tx)(opt)
        case _ =>  // XXX TODO -- should we ask for expr.value ?
      }}
      val v = valObs.swap(res)
      if (v != null) v.dispose()
    }

    private def valueRemoved()(implicit tx: T): Boolean = {
      val v   = valObs.swap(null)
      val res = v != null
      if (res) v.dispose()
      res
    }

    def dispose()(implicit tx: T): Unit = {
      valueRemoved()
      mapObs.dispose()
    }
  }

  private final class ExprMap[T <: Txn[T], K, A, _Ex[~ <: Txn[~]] <: _Expr[~, A] /* , U */](
                                                                                             protected val h: Source[T, MapObj[T, K, _Ex]],
                                                                                             protected val key: K /* , val updFun: U => Option[A] */)
    extends ExprMapObjLike[T, K, A, _Ex /* , U */] {

    override def repr(implicit tx: T): Repr = h().get(key)

    // protected def mapUpdate(u: U): Option[A] = updFun(u)
  }

  private final class ExprModMap[T <: Txn[T], K, A,
    _Ex[~ <: Txn[~]] <: _Expr[~, A]](protected val h: Source[T, MapObj.Modifiable[T, K, _Ex]], protected val key: K)
                                    (implicit tpe: _Expr.Type[A, _Ex])
    extends ExprMapObjLike[T, K, A, _Ex] with CellView.VarR[T, Option[A]] {

    def format: TFormat[T, Repr] = {
      implicit val exFmt: TFormat[T, _Ex[T]] = tpe.format[T]
      TFormat.option[T, _Ex[T]]
    }

    // protected def mapUpdate(ch: Change[A]): Option[A] = if (ch.isSignificant) Some(ch.now) else None

    override def repr(implicit tx: T): Repr = {
      val opt = h().get(key)
      // ! important to unwrap, otherwise we get infinite recursion with `repr = repr` !
      opt.map {
        case tpe.Var(vr) =>
          vr()
        case other => other
      }
    }

    def repr_=(value: Repr)(implicit tx: T): Unit = value.fold[Unit] {
      h().remove(key)
    } { ex =>
      val map = h()
      map.get(key) match {
        case Some(tpe.Var(vr)) => vr() = ex
        case _ =>
          val exV = tpe.Var.unapply(ex).getOrElse(tpe.newVar(ex))
          map.put(key, exV)
      }
    }

    def lift(value: Option[A])(implicit tx: T): Repr = value.map(tpe.newConst[T](_))

    def update(v: Option[A])(implicit tx: T): Unit = repr_=(lift(v))
  }

  private[lucre] trait ExprLike[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: _Expr[~, A]]
    extends CellView[T, A] {

    type Repr = _Ex[T]

    protected def h: Source[T, Repr]

    def react(fun: T => A => Unit)(implicit tx: T): Disposable[T] =
      h().changed.react { implicit tx => ch => fun(tx)(ch.now) }

    def apply()(implicit tx: T): A = h().value
  }

  private final class Expr[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](
                                                                             protected val h: Source[T, _Ex[T]])
    extends ExprLike[T, A, _Ex]

  private final class ExprVar[T <: Txn[T], A, _Ex[~ <: Txn[~]] <: _Expr[~, A]](
                                                                                protected val h: Source[T, _Ex[T] with LVar[T, _Ex[T]]])
                                                                              (implicit tpe: _Expr.Type[A, _Ex])
    extends ExprLike[T, A, _Ex] with CellView.VarR[T, A] {

    // ! important to unwrap, otherwise we get infinite recursion with `repr = repr` !
    override def repr(implicit tx: T): Repr = h().apply()

    def repr_=(value: Repr)(implicit tx: T): Unit = h().update(value)

    def lift(value: A)(implicit tx: T): Repr = tpe.newConst(value)

    def update(v: A)(implicit tx: T): Unit = repr_=(lift(v))

    def format: TFormat[T, Repr] = tpe.format
  }

  private[lucre] final class MapImpl[Tx, A, B](in: CellView[Tx, A], f: A => B)
    extends CellView[Tx, B] {

    def react(fun: Tx => B => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => a => fun(tx)(f(a)) }

    def apply()(implicit tx: Tx): B = f(in())
  }

  private[lucre] final class OptionOrElseImpl[Tx, A](first : CellView[Tx, Option[A]],
                                                     second: CellView[Tx, Option[A]])
    extends CellView[Tx, Option[A]] {

    def apply()(implicit tx: Tx): Option[A] =
      first() orElse second()

    def react(fun: Tx => Option[A] => Unit)(implicit tx: Tx): Disposable[Tx] = {
      val r1 = first  .react { implicit tx => opt => fun(tx)(opt      orElse second() ) }
      val r2 = second .react { implicit tx => opt => fun(tx)(first()  orElse opt      ) }
      Disposable.seq(r1, r2)
    }
  }

  private[lucre] final class FlatMapImpl[Tx, A, B](in: CellView[Tx, Option[A]], f: Tx => A => Option[B])
    extends CellView[Tx, Option[B]] {

    def apply()(implicit tx: Tx): Option[B] =
      in().flatMap(f(tx)(_))

    def react(fun: Tx => Option[B] => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => aOpt =>
        fun(tx)(aOpt.flatMap(f(tx)(_)))
      }
  }

  private[lucre] final class CatVarImpl[Tx, A, B](in: CellView[Tx, Option[A]])(cat: Tx => A => CellView.Var[Tx, Option[B]])
    extends CellView.Var[Tx, Option[B]] {

    def apply()(implicit tx: Tx): Option[B] =
      in().flatMap(cat(tx)(_).apply())

    def update(v: Option[B])(implicit tx: Tx): Unit =
      in().foreach(cat(tx)(_).update(v))

    def react(fun: Tx => Option[B] => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => aOpt =>
        fun(tx)(aOpt.flatMap(cat(tx)(_).apply()))
      }
  }

  private final class Const[Tx, A](value: A)
    extends CellView[Tx, A] {

    def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx] = Disposable.empty

    def apply()(implicit tx: Tx): A = value
  }

  private[lucre] trait AttrBasic[T <: Txn[T], A, E[~ <: Txn[~]] <: _Expr[~, A]]
    extends CellView[T, Option[A]] {

    protected def h: Source[T, Obj.AttrMap[T]]
    protected val key: String

    // implicit protected def companion: Elem.Companion[E]
    implicit protected val tpe: _Expr.Type[A, E]

    //    implicit protected def classTag: ClassTag[E[T]]

    final type Repr = Option[E[T]] // Expr[T, A]]

    def react(fun: T => Option[A] => Unit)(implicit tx: T): Disposable[T] =
      new AttrMapExprObs[T, A](map = h(), key = key, fun = fun, tx0 = tx)

    def repr(implicit tx: T): Repr = {
      val opt = h().get(key)
      opt match {
        case Some(v) if v.tpe.typeId == tpe.typeId =>
          val vt = v.asInstanceOf[E[T]]
          val vv = vt match {
            case tpe.Var(vr)  => vr()
            case other        => other
          }
          Some(vv)

        case _ => None
      }
    }

    def apply()(implicit tx: T): Option[A] = repr.map(_.value)
  }

  final class AttrMapExprObs[T <: Txn[T], A](map: Obj.AttrMap[T], key: String, fun: T => Option[A] => Unit,
                                             tx0: T)(implicit tpe: Obj.Type)
    extends MapObjLikeExprObs[T, A, Obj](map, key, fun, tx0) {

    protected def compareTpe(value: Obj[T]): Boolean =
      value.tpe == tpe
  }

  // XXX TODO --- lot's of overlap with CellViewImpl
  /** N.B.: `tpe` must denote objects that extend `Expr`, otherwise we get class-cast exceptions. */
  abstract class MapObjLikeExprObs[T <: Txn[T], A, 
    Repr[~ <: Txn[~]] <: Form[~]](map: MapObjLike[T, String, Repr[T]], key: String, fun: T => Option[A] => Unit, tx0: T)
    extends Disposable[T] {

    private[this] val valObs = Ref(null: Disposable[T])

    protected def compareTpe(in: Repr[T]): Boolean

    private[this] def obsAdded(value: Repr[T])(implicit tx: T): Unit = {
      val valueT = value.asInstanceOf[_Expr[T, A]]
      valueAdded(valueT)
      // XXX TODO -- if we moved this into `valueAdded`, the contract
      // could be that initially the view is updated
      val now0 = valueT.value
      fun(tx)(Some(now0))
    }

    @inline
    private[this] def obsRemoved()(implicit tx: T): Unit =
      if (valueRemoved()) fun(tx)(None)

    private[this] val mapObs = map.changed.react { implicit tx => u =>
      u.changes.foreach {
        case Obj.AttrAdded   (`key`, value) if compareTpe(value) => obsAdded  (value)
        case Obj.AttrRemoved (`key`, value) if compareTpe(value) => obsRemoved()
        case Obj.AttrReplaced(`key`, before, now) =>
          if      (compareTpe(now    )) obsAdded(now)
          else if (compareTpe(before )) obsRemoved()
        case _ =>
      }
    } (tx0)

    map.get(key)(tx0).foreach { value =>
      if (compareTpe(value)) valueAdded(value.asInstanceOf[_ExprLike[T, A]])(tx0)
    }

    private[this] def valueAdded(value: _ExprLike[T, A])(implicit tx: T): Unit = {
      val res = value.changed.react { implicit tx => {
        case Change(_, now) =>
          fun(tx)(Some(now))
        //            val opt = mapUpdate(ch)
        //            if (opt.isDefined) fun(tx)(opt)
        case _ =>  // XXX TODO -- should we ask for expr.value ?
      }}
      val v = valObs.swap(res)(tx.peer)
      if (v != null) v.dispose()
    }

    private[this] def valueRemoved()(implicit tx: T): Boolean = {
      val v   = valObs.swap(null)(tx.peer)
      val res = v != null
      if (res) v.dispose()
      res
    }

    def dispose()(implicit tx: T): Unit = {
      valueRemoved()
      mapObs.dispose()
    }
  }

  private[lucre] abstract class AttrImpl[T <: Txn[T], A,
    E[~ <: Txn[~]] <: _Expr[~, A]](protected val h: Source[T, Obj.AttrMap[T]],
                                   protected val key: String)(implicit val tpe: _Expr.Type[A, E])
    extends AttrBasic[T, A, E] with CellView.VarR[T, Option[A]] {

    final type EVar[~ <: Txn[~]] = tpe.Var[~]

    // ---- abstract ----

    protected def putImpl   (map: Obj.AttrMap[T], value: E[T])(implicit tx: T): Unit
    protected def removeImpl(map: Obj.AttrMap[T]             )(implicit tx: T): Unit
    protected def updateVarImpl(vr: EVar[T]     , value: E[T])(implicit tx: T): Unit

    // ---- impl ----

    final def format: TFormat[T, Repr] = {
      implicit val exFmt: TFormat[T, E[T]] = tpe.format[T]
      TFormat.option[T, E[T]]
    }

    protected final def mapUpdate(ch: Change[A]): Option[A] = if (ch.isSignificant) Some(ch.now) else None

    private def overwrite(map: Obj.AttrMap[T], value: E[T])(implicit tx: T): Unit = {
      val aObj = tpe.Var.unapply[T](value).getOrElse(tpe.newVar(value))
      putImpl(map, aObj)
    }

    def repr_=(value: Repr)(implicit tx: T): Unit = value.fold[Unit] {
      val map = h()
      removeImpl(map)
    } { ex =>
      val map = h()
      val opt = map.get(key)
      opt match {
        case Some(v) if v.tpe.typeId == tpe.typeId =>
          val vt = v.asInstanceOf[E[T]]
          vt match {
            case tpe.Var(vr) => updateVarImpl(vr, ex)
            case _ => overwrite(map, ex)
          }

        case _ => overwrite(map,ex)
      }
    }

    def lift(value: Option[A])(implicit tx: T): Repr =
      value.map(v => tpe.newConst[T](v))

    def update(v: Option[A])(implicit tx: T): Unit = repr_=(lift(v))
  }

  // plain mutating implementation (no undo support)
  private final class PlainAttrImpl[T <: Txn[T], A,
    E[~ <: Txn[~]] <: _Expr[~, A]](h: Source[T, Obj.AttrMap[T]], key: String)(implicit tpe: _Expr.Type[A, E])
    extends AttrImpl[T, A, E](h, key) {

    protected def putImpl(map: AttrMap[T], value: E[T])(implicit tx: T): Unit =
      map.put(key, value)

    protected def removeImpl(map: AttrMap[T])(implicit tx: T): Unit =
      map.remove(key)

    protected def updateVarImpl(vr: EVar[T], value: E[T])(implicit tx: T): Unit =
      vr() = value    // IntelliJ highlight bug
  }

  // adding optional undo support (when present)
  private final class UndoAttrImpl[T <: Txn[T], A, E[~ <: Txn[~]] <: _Expr[~, A]](
                                                                                   h: Source[T, Obj.AttrMap[T]], key: String)(implicit tpe: _Expr.Type[A, E])
    extends AttrImpl[T, A, E](h, key) {

    protected def putImpl(map: AttrMap[T], value: E[T])(implicit tx: T): Unit =
      EditAttrMap.put(map, key, value)

    protected def removeImpl(map: AttrMap[T])(implicit tx: T): Unit =
      EditAttrMap.remove(map, key)

    protected def updateVarImpl(vr: EVar[T], value: E[T])(implicit tx: T): Unit =
      EditExprVar.apply[T, A, E](vr, value)  // IntelliJ highlight bug
  }
}
