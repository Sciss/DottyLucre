/*
 *  Graph.scala
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

import de.sciss.lucre.expr.graph.{Control, It}
import de.sciss.lucre.expr.impl.{ExElem, GraphBuilderMixin, GraphFormatMixin}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object Graph {
  trait Builder {
    def addControl  (c: Control): Unit
    def putProperty (c: Control, key: String, value: Any): Unit

    def allocToken[A](): It[A]
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder = builderRef.get()

  private[this] val builderRef: ThreadLocal[Builder] = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    def addControl(c: Control): Unit = ()

    def putProperty(c: Control, key: String, value: Any): Unit = ()

    def allocToken[A](): It[A] = outOfContext
  }

  def apply(thunk: => Any): Graph = {
    val b = new BuilderImpl
    use(b) {
      thunk
      b.build()
    }
  }

  def withResult[A](thunk: => A): (Graph, A) = {
    val b   = new BuilderImpl
    use(b) {
      val ex = thunk
      val g  = b.build()
      (g, ex)
    }
  }

  def use[A](b: Builder)(body: => A): A = {
    val old = builderRef.get()
    builderRef.set(b)
    try {
      body
    } finally {
      builderRef.set(old)
    }
  }

  private[this] final class BuilderImpl extends GraphBuilderMixin {
    override def toString = s"lucre.expr.Graph.Builder@${hashCode.toHexString}"
  }

  implicit object format extends ConstFormat[Graph] with GraphFormatMixin {
    private final val SER_VERSION = 0x4378  // "Cx"

    def write(g: Graph, out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      val ref = null: ExElem.RefMapOut
      val cx = g.controls
      writeControls(cx, out, ref)
    }

    def read(in: DataInput): Graph = {
      val cookie = in.readShort()
      require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
      val ref = new ExElem.RefMapIn
      val cx = readControls(in, ref)
      Graph(cx)
    }
  }

  private final class ExpandedImpl[T <: Txn[T]](controls: ISeq[IControl[T]] /*, disposable: Disposable[T]*/ )
    extends IControl[T] {

    def initControl()(implicit tx: T): Unit =
      controls.foreach(_.initControl())

    def dispose()(implicit tx: T): Unit =
      controls.foreach(_.dispose())
  }

  val empty: Graph = Graph(Vector.empty)

  def apply(controls: Vec[Control.Configured]): Graph = Impl(controls)

  private final case class Impl(controls: Vec[Control.Configured]) extends Graph {

    override def productPrefix: String = "Graph"

    def expand[T <: Txn[T]](implicit tx: T, ctx: Context[T]): IControl[T] = {
      if (controls.isEmpty) IControl.empty[T] else {
        ctx.initGraph(this)
        val iControls = controls.map(_.control.expand[T])
        new Graph.ExpandedImpl[T](iControls)
      }
    }
  }
}
trait Graph extends Product {
  def controls: Vec[Control.Configured]

  /** Expands the graph and unites all disposables and controls under the returned control value.
   *
   * ''Important:'' disposing the resulting `IControl` does not clean up any `IExpr` which are only
   * captured by the provided `Context`. Therefore, to dispose the entire graph, it is crucial
   * to make sure that `ctx` is disposed. This will automatically include all controls, so there
   * is no need to dispose the returned `IControl` in that case.
   */
  def expand[T <: Txn[T]](implicit tx: T, ctx: Context[T]): IControl[T]
}