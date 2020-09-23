/*
 *  It.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.TxnLike.peer
import de.sciss.lucre.expr.{Context, graph}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object It {
  trait Expanded[T <: Txn[T], A] extends IExpr[T, A] {
    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: T): Unit

    def ref: AnyRef
  }

  private final class ExpandedImpl[T <: Txn[T], A](val ref: AnyRef)(implicit protected val targets: ITargets[T])
    extends Expanded[T, A] with IChangeGeneratorEvent[T, A] {

    private[this] val valueRef = Ref.make[A]()

    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: T): Unit = {
      valueRef() = value
//      val old =  ref.swap(value)
//      if (/*dispatch && */ old != value) {
//        fire(Change(old, value))
//      }
    }


    override private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
      throw new IllegalArgumentException("pullUpdate on It.Expanded")

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      value // pull.resolveChange(isNow = isNow) // throw new AssertionError("Should never be here")
    }

    //    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
//      Some(pull.resolve)

    def value(implicit tx: T): A = valueRef()

    def dispose()(implicit tx: T): Unit = ()

    def changed: IChangeEvent[T, A] = this
  }
}
/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Ex[A] {

  type Repr[T <: Txn[T]] = It.Expanded[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new graph.It.ExpandedImpl[T, A](ref)
  }
}
