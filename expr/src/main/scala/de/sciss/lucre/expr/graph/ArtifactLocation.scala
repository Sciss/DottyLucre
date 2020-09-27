/*
 *  ArtifactLocation.scala
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

import de.sciss.file._
import de.sciss.lucre.Obj.AttrMap
import de.sciss.lucre.edit.{EditAttrMap, EditExprVar}
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Context}
import de.sciss.lucre.{Adjunct, Disposable, IExpr, ProductWithAdjuncts, Source, Txn, ArtifactLocation => _ArtifactLocation, Obj => LObj}
import de.sciss.serial.DataInput

object ArtifactLocation {
  private lazy val _init: Unit = Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final object Bridge extends Obj.Bridge[File] with Adjunct.Factory {
    final val id = 2003

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[File]] =
      new ObjCellViewImpl(tx.newHandle(obj.attr), key = key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[File]] =
      new AbstractCtxCellView[T, File](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[File] = value match {
          case f: File  => Some(f)
          case _        => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[File] = obj match {
          case a: _ArtifactLocation[T]  => Some(a .value)
          case _                        => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[File] =
      obj.attr.$[_ArtifactLocation](key).map(_.value)

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[File] = obj match {
      case a: _ArtifactLocation[T]  => Some(a.value)
      case _                        => None
    }
  }

  private final class ObjCellViewImpl[T <: Txn[T]](attrH: Source[T, AttrMap[T]], key: String)
    extends CellView.Var[T, Option[File]] {

    private def attr(implicit tx: T): AttrMap[T] = attrH()

    private type Repr = Option[_ArtifactLocation[T]]

    private def repr(implicit tx: T): Repr =
      attr.$[_ArtifactLocation](key)

    private def putImpl(map: AttrMap[T], value: _ArtifactLocation[T])(implicit tx: T): Unit =
      EditAttrMap.put(map, key, value)

    private def removeImpl(map: AttrMap[T])(implicit tx: T): Unit =
      EditAttrMap.remove(map, key)

    private def repr_=(value: Repr)(implicit tx: T): Unit =
      value match {
        case Some(a)  => putImpl(attr, a)
        case None     => removeImpl(attr)
      }

    private def lift(v: Option[File])(implicit tx: T): Repr =
      v match {
        case Some(f) if f.path.nonEmpty =>
          val loc = _ArtifactLocation.newVar[T](f)
          Some(loc)

        case _ => None
      }

    def apply()(implicit tx: T): Option[File] = repr.map(_.value)

    def update(v: Option[File])(implicit tx: T): Unit = {
      def fallback(): Unit = repr_=(lift(v))

      v match {
        case Some(f) if f.path.nonEmpty =>
          repr match {
            case Some(_ArtifactLocation.Var(am)) =>
              EditExprVar[T, File, _ArtifactLocation](am, f)
            case _ => fallback()
          }
        case _ => fallback()
      }
    }

    def react(fun: T => Option[File] => Unit)(implicit tx: T): Disposable[T] =
      new AttrMapExprObs[T, File](map = attr, key = key, fun = fun, tx0 = tx)(_ArtifactLocation)
  }
}
final case class ArtifactLocation(key: String, default: Ex[File] = file(""))
  extends Attr.WithDefault[File] with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, File]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    val defaultEx: Repr[T] = default.expand[T]
    val attrView = Attr.resolveNested(key)
    import ctx.targets
    new Attr.WithDefault.Expanded[T, File](key, attrView, defaultEx, tx)
  }

  def update(in: Ex[File]): Control = Attr.Update (in, key)
  def set   (in: Ex[File]): Act     = Attr.Set    (in, key)

  implicit private def bridge: Obj.Bridge[File] = ArtifactLocation.Bridge

  def adjuncts: List[Adjunct] = Nil
}
