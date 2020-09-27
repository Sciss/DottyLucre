/*
 *  Obj.scala
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

import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedAttrSetIn, ExpandedAttrUpdateIn, MappedIExpr, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl, ExSeqObjBridgeImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction, IControl}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, BooleanObj, Caching, Disposable, DoubleObj, DoubleVector, IChangeEvent, IExpr, IPull, IPush, ITargets, IntObj, IntVector, LongObj, ProductWithAdjuncts, SpanLikeObj, SpanObj, StringObj, Sys, Txn, Obj => LObj, Source => LSource}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.concurrent.stm.Ref

object Obj {
  private lazy val _init: Unit = {
    Adjunct.addFactory(Source.obj)
    Adjunct.addFactory(Bridge.obj)
  }

  def init(): Unit = _init

  implicit final class ExOps(private val obj: Ex[Obj]) extends AnyVal {
    def attr[A: Bridge](key: String): Obj.Attr[A] = Obj.Attr(obj, key)

    // def attr[A: Bridge](key: String, default: Ex[A]): _Attr.WithDefault[A] = ...

    def copy: Copy = Obj.Copy(obj)

    /** Tries to parse this object as a more specific type.
      * Produces `Some` if the type matches, otherwise `None`.
      */
    def as[A: Bridge]: Ex[Option[A]] = As[A](obj)
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[T <: Txn[T]](peer: LSource[T, LObj[T]], system: Sys): Obj =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: LObj[T])(implicit tx: T): Obj =
    new Impl[T](tx.newHandle(peer), tx.system)

  def empty: Ex[Obj] = Const(Empty)

  private[lucre] case object Empty extends Obj {
    override def productPrefix: String = s"Obj$$Empty$$"  // serialization
    override def toString     : String = "Obj<empty>"

    type Peer[~ <: Txn[~]] = LObj[~]

    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] =
      None // throw new IllegalStateException("Object has not been created yet")
  }

  private final class Impl[In <: Txn[In]](in: LSource[In, LObj[In]], system: Sys)
    extends ObjImplBase[In, LObj](in, system) {

    override def toString: String = s"Obj($in)"
  }

  private abstract class AbstractMakeExpanded[T <: Txn[T]]
    extends IExpr[T, Obj]
      with IAction[T]
      with IChangeGeneratorEvent [T, Obj]
      with ITriggerConsumer [T, Obj]
      with Caching {

    // ---- abstract ----

    protected def make()(implicit tx: T): Obj

    // ---- impl ----

    private[this] val ref = Ref[Obj](Empty)

    final def value(implicit tx: T): Obj =
      IPush.tryPull(this).fold(ref())(_.now)

    final def executeAction()(implicit tx: T): Unit =
      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;

    final protected def trigReceived()(implicit tx: T): Obj = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: T): Obj = ref()

    final def changed: IChangeEvent[T, Obj] = this
  }

  private final class MakeExpanded[T <: Txn[T], A](ex: IExpr[T, A])(implicit protected val targets: ITargets[T],
                                                                    cm: CanMake[A])
    extends AbstractMakeExpanded[T] {

    protected def make()(implicit tx: T): Obj = {
      val v     = ex.value
      val peer  = cm.toObj(v)
      wrap(peer)
    }
  }

  object Make {
    def apply[A](ex: Ex[A])(implicit cm: CanMake[A]): Make = Impl(ex)

    private final case class Impl[A](ex: Ex[A])(implicit cm: CanMake[A]) 
      extends Make with Act with ProductWithAdjuncts {
      
      type Repr[T <: Txn[T]] = IExpr[T, Obj] with IAction[T]

      override def productPrefix: String = s"Obj$$Make" // serialization

      def make: Act = this

      def adjuncts: List[Adjunct] = cm :: Nil

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new MakeExpanded(ex.expand[T])
      }
    }
  }
  trait Make extends Ex[Obj] {
    def make: Act
  }

  implicit def hasDefault: HasDefault[Obj] = Bridge.obj

  object Bridge {
    implicit val int      : Bridge[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : Bridge[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : Bridge[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : Bridge[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : Bridge[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : Bridge[_SpanLike  ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : Bridge[_Span      ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : Bridge[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: Bridge[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )

    implicit object obj extends Bridge[Obj] with HasDefault[Obj] with Adjunct.Factory {
      final val id = 1005

      type Repr[T <: Txn[T]] = LObj[T]

      def defaultValue: Obj = Empty

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this

      def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Obj]] =
        new ObjCellViewVarImpl[T, LObj, Obj](tx.newHandle(obj), key) {
          protected def lower(peer: LObj[T])(implicit tx: T): Obj =
            wrap(peer)
        }

      def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Obj]] = {
        new AbstractCtxCellView[T, Obj](context.attr, key) {
          protected def tryParseValue(value: Any)(implicit tx: T): Option[Obj] = value match {
            case obj: Obj => Some(obj)
            case _        => None
          }

          protected def tryParseObj(peer: LObj[T])(implicit tx: T): Option[Obj] =
            Some(wrap(peer))
        }
      }

      def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Obj] =
        obj.attr.get(key).map(wrap(_))

      def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Obj] =
        Some(wrap(obj))
    }
  }
  trait Bridge[A] extends Adjunct {
    /** Creates a bidirectional view between `LObj` and the expression side representation type `A`.
      * If possible, implementations should look at `UndoManager.find` when updating values.
      */
    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[A]]

    /** Creates a unidirectional view between a context's attribute or self object and the expression side
      * representation type `A`.
      */
    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[A]]

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[A]

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[A]
  }

  object Source {
    implicit object obj extends Source[Obj] with Adjunct.Factory {
      final val id = 1006

      type Repr[T <: Txn[T]] = LObj[T]

      def toObj[T <: Txn[T]](value: Obj)(implicit tx: T): LObj[T] =
        value.peer.getOrElse(throw new IllegalStateException("Object has not yet been instantiated"))

      implicit def reprTFormat[T <: Txn[T]]: TFormat[T, LObj[T]] = LObj.format

      def readIdentifiedAdjunct(in: DataInput): Adjunct = this
    }

    implicit def canMake[A](implicit peer: CanMake[A]): Source[A] = peer
  }

  /** An `Obj.Source` either has an `LObj` peer, or it can make one.
    * The latter is represented by sub-trait `CanMake`.
    */
  trait Source[-A] extends Adjunct {
    type Repr[T <: Txn[T]] <: LObj[T]

    def toObj[T <: Txn[T]](value: A)(implicit tx: T): Repr[T]

    implicit def reprTFormat[T <: Txn[T]]: TFormat[T, Repr[T]]
  }

  object CanMake {
    implicit val int      : CanMake[Int        ] = new ExObjBridgeImpl(IntObj       )
    implicit val long     : CanMake[Long       ] = new ExObjBridgeImpl(LongObj      )
    implicit val double   : CanMake[Double     ] = new ExObjBridgeImpl(DoubleObj    )
    implicit val boolean  : CanMake[Boolean    ] = new ExObjBridgeImpl(BooleanObj   )
    implicit val string   : CanMake[String     ] = new ExObjBridgeImpl(StringObj    )
    implicit val spanLike : CanMake[_SpanLike  ] = new ExObjBridgeImpl(SpanLikeObj  )
    implicit val span     : CanMake[_Span      ] = new ExObjBridgeImpl(SpanObj      )
    implicit val intVec   : CanMake[Seq[Int   ]] = new ExSeqObjBridgeImpl(IntVector    )
    implicit val doubleVec: CanMake[Seq[Double]] = new ExSeqObjBridgeImpl(DoubleVector )
  }
  trait CanMake[A] extends Source[A]

  private final class AttrExpanded[T <: Txn[T], A](obj: IExpr[T, Obj], key: String, tx0: T)
                                                  (implicit protected val targets: ITargets[T], bridge: Bridge[A])
    extends IExpr[T, Option[A]] with IChangeGeneratorEvent[T, Option[A]] {

    override def toString: String = s"graph.Obj.AttrExpanded($obj, $key)@${hashCode().toHexString}"

    private[this] val viewRef   = Ref(Option.empty[CellView.Var[T, Option[A]]])
    private[this] val valueRef  = Ref.make[Option[A]]()
    private[this] val obsRef    = Ref.make[Disposable[T]]()
    private[this] val objObs    = obj.changed.react { implicit tx => upd =>
      setObj(upd.now, init = false)
    } (tx0)

    private def setNewValue(now: Option[A])(implicit tx: T): Unit = {
      val before = valueRef.swap(now)
      if (before != now) {
        fire(Change(before, now))
      }
    }

    private def setObj(newObj: Obj, init: Boolean)(implicit tx: T): Unit = {
      // println(s"newObj = $newObj, bridge = $bridge, key = $key")
      val newView = newObj.peer.map(p => bridge.cellView(p, key))
      viewRef()   = newView
      val obsNew = newView.fold[Disposable[T]](Disposable.empty)(_.react { implicit tx => now =>
        setNewValue(now)
      })
      val now: Option[A] = newView.flatMap(_.apply())
      if (init) {
        obsRef  () = obsNew
        valueRef() = now
      } else {
        obsRef.swap(obsNew).dispose()
        setNewValue(now)
      }
    }

    // ---- init ----
    setObj(obj.value(tx0), init = true)(tx0)

    def value(implicit tx: T): Option[A] = viewRef().flatMap(_.apply())

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Option[A] =
      pull.resolveExpr(this)

    def changed: IChangeEvent[T, Option[A]] = this

    def dispose()(implicit tx: T): Unit = {
      objObs  .dispose()
      obsRef().dispose()
    }
  }

  object Attr {
    final case class Update[A](obj: Ex[Obj], key: String,value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Control with ProductWithAdjuncts {

      override def productPrefix: String = s"Obj$$Attr$$Update"  // serialization

      type Repr[T <: Txn[T]] = IControl[T]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val peer = new ExpandedAttrUpdateIn[T, A](obj.expand[T], key, value.expand[T], tx)
        IControl.wrap(peer)
      }

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }
    final case class Set[A](obj: Ex[Obj], key: String, value: Ex[A])(implicit bridge: Obj.Bridge[A])
      extends Act with ProductWithAdjuncts {

      override def productPrefix: String = s"Obj$$Attr$$Set"  // serialization

      type Repr[T <: Txn[T]] = IAction[T]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
        new ExpandedAttrSetIn[T, A](obj.expand[T], key, value.expand[T], tx)

      override def adjuncts: scala.List[Adjunct] = bridge :: Nil
    }
  }

  // XXX TODO --- this should be merged with graph.Attr ?
  final case class Attr[A](obj: Ex[Obj], key: String)(implicit val bridge: Bridge[A])
    extends Ex[Option[A]] with _Attr.Like[A] with ProductWithAdjuncts {

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    override def productPrefix: String = s"Obj$$Attr" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new AttrExpanded(obj.expand[T], key, tx)
    }

    def update(in: Ex[A]): Control  = Obj.Attr.Update (obj, key, in)
    def set   (in: Ex[A]): Act      = Obj.Attr.Set    (obj, key, in)

    def adjuncts: List[Adjunct] = bridge :: Nil
  }

  private final class CopyExpanded[T <: Txn[T]](ex: IExpr[T, Obj])(implicit protected val targets: ITargets[T])
    extends AbstractMakeExpanded[T] {

    protected def make()(implicit tx: T): Obj = {
      val v = ex.value
      v.peer match {
        case Some(orig) =>
          val cpy: LObj[T] = LObj.copy(orig)
          wrap(cpy)

        case None => Empty
      }
    }
  }

  object Copy {
    def apply(obj: Ex[Obj]): Copy = Impl(obj)

    private final case class Impl(obj: Ex[Obj])
      extends Copy with Act {

      type Repr[T <: Txn[T]] = IExpr[T, Obj] with IAction[T]

      def make: Act = this

      override def productPrefix: String = s"Obj$$Copy" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new CopyExpanded[T](obj.expand[T])
      }
    }
  }
  type Copy = Make
  // trait Copy extends Make

  object As {
    def apply[A: Bridge](obj: Ex[Obj]): Ex[Option[A]] = Impl(obj)

    // XXX TODO --- we should use cell-views instead, because this way we won't notice
    // changes to the value representation (e.g. a `StringObj.Var` contents change)
    private final class Expanded[T <: Txn[T], A](in: IExpr[T, Obj], tx0: T)
                                                           (implicit targets: ITargets[T], bridge: Obj.Bridge[A])
      extends MappedIExpr[T, Obj, Option[A]](in, tx0) {

      protected def mapValue(inValue: Obj)(implicit tx: T): Option[A] =
        inValue.peer.flatMap(bridge.tryParseObj(_))
    }

    private final case class Impl[A](obj: Ex[Obj])(implicit val bridge: Bridge[A])
      extends Ex[Option[A]] with ProductWithAdjuncts {

      override def toString: String = s"$obj.as[$bridge]"

      type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

      def adjuncts: List[Adjunct] = bridge :: Nil

      override def productPrefix: String = s"Obj$$As" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new Expanded[T, A](obj.expand[T], tx)
      }
    }
  }
}
trait Obj {
  type Peer[~ <: Txn[~]] <: LObj[~]

  private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]]
}
