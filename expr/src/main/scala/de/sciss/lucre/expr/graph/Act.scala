/*
 *  Act.scala
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
package expr
package graph

import de.sciss.lucre.expr.impl.IActionImpl

object Act {
  def apply(xs: Act*): Act = SeqImpl(xs)

  /** Creates an action that can link or loop back to itself.
    * It does so by calling the function `f` with a trigger
    * which is then linked to the output of `f`. This is useful
    * for creating delays and feedback.
    *
    * Example:
    *
    * {{{
    * val metro = Act.link { self =>
    *   Act(
    *     PrintLn("bang"),
    *     Delay(1.0)(self)
    *   )
    * }
    * }}}
    */
  def link(f: Act => Act): Act = {
    val in  = Trig()
    val out = f(in)
    in ---> out
    in
  }

  private final class ExpandedSeq[T <: Txn[T]](xs: Seq[IAction[T]]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      xs.foreach(_.executeAction())
  }

  private final case class SeqImpl(xs: Seq[Act]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix = "Act" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedSeq(xs.map(_.expand[T]))
  }

  final case class Link[A](source: Trig, sink: Act)
    extends Control {

    override def productPrefix = s"Act$$Link" // serialization

    type Repr[T <: Txn[T]] = IControl[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val tr    = source.expand[T]
      val ac    = sink  .expand[T]
      ac.addSource(tr)
//      val peer  = tr.changed.react { implicit tx => _ => ac.executeAction() }
      IControl.empty // .wrap(peer)
    }
  }

//  /** Treats an expression of actions as an action, simply
//    * expanding its value every time it is called.
//    */
//  implicit def flatten(in: Ex[Act]): Act = Flatten(in)

//  trait Option[T <: Txn[T]] extends IExpr[T, _Option[Act]] {
//    def executeAction()(implicit tx: T): Boolean
//  }

  trait Option extends Act {
    type Repr[T <: Txn[T]] <: IAction.Option[T]
  }

  implicit final class OptionOps(private val in: Act.Option) extends AnyVal {
    //    def isEmpty   : Ex[Boolean] = ...
    //    def isDefined : Ex[Boolean] = ...

    def orElse(that: Act): Act = OrElse(in, that)
  }

//  private final class ExpandedFlatten[T <: Txn[T]](in: IExpr[T, Act])(implicit ctx: Context[T])
//    extends IActionImpl[T] {
//
//    def executeAction()(implicit tx: T): Unit = {
//      val act = in.value
//      // XXX TODO --- nesting produces all sorts of problems
//      // why did we try to do that in the first place?
////      val (actEx, d) = ctx.nested {
////        act.expand[T]
////      }
//      val actEx = act.expand[T]
//      actEx.executeAction()
////      d.dispose()
//    }
//  }

  private final class ExpandedOrElse[T <: Txn[T]](a: IAction.Option[T], b: IAction[T])/*(implicit ctx: Context[T])*/
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val c = if (a.isDefined) a else b
      c.executeAction()
    }
  }

  final case class OrElse(a: Act.Option, b: Act) extends Act {
    override def productPrefix = s"Act$$OrElse" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedOrElse(a.expand[T], b.expand[T])
  }

//  final case class Flatten(in: Ex[Act]) extends Act {
//
//    override def productPrefix = s"Act$$Flatten" // serialization
//
//    type Repr[T <: Txn[T]] = IAction[T]
//
//    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
//      new ExpandedFlatten(in.expand[T])
//  }

  private final class ExpandedNop[T <: Txn[T]]/*(implicit ctx: Context[T])*/
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = ()
  }

  final case class Nop() extends Act {

    override def productPrefix = s"Act$$Nop" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedNop
  }
}
trait Act extends Lazy {
  type Repr[T <: Txn[T]] <: IAction[T]
}
