/*
 *  ExpandedMapExSeqOrOption.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{Caching, Disposable, IChangeEvent, IExpr, IPull, IPush, ITargets, Txn}

import scala.collection.mutable
import scala.concurrent.stm.Ref

abstract class ExpandedMapSeqOrOption[T <: Txn[T], A, In[_], P, Out](in: IExpr[T, In[A]], it: It.Expanded[T, A],
                                                                     /* closure: Graph, */ fun: Ex[P], tx0: T)
                                                                    (implicit protected val targets: ITargets[T], ctx: Context[T])
  extends IExpr[T, Out] with IChangeEventImpl[T, Out] with Caching {

  protected type Tuples = In[(IExpr[T, P], Disposable[T])]

  // ---- abstract: in ----

  protected def isEmpty[A1](in: In[A1]): Boolean

  protected def foreach[A1](in: In[A1])(body: A1 => Unit): Unit

  protected def emptyIn[A1]: In[A1]

  protected def emptyOut: Out

  protected def map[A1, B1](in: In[A1])(body: A1 => B1): In[B1]

  protected def buildResult(inV: In[A], tuples: Tuples)(elem: IExpr[T, P] => P)(implicit tx: T): Out

  // ---- impl ----

  private val ref = Ref.make[Tuples]()

  private def init()(implicit tx: T): Unit = {
    in .changed.--->(this)
    val inV   = in.value
    val refV  = mkRef(inV)
    ref()     = refV
  }

  init()(tx0)

  private def disposeRef(refV: In[(IExpr[T, P], Disposable[T])])(implicit tx: T): Unit =
    foreach(refV) { case (f, d) =>
      f.changed -/-> this
      d.dispose()
    }

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Out = {
    val inV = pull.expr(in)
    if (pull.contains(in.changed) && phase.isNow) {
      val refV = mkRef(inV)
      disposeRef(ref.swap(refV))
    }
    if (isEmpty(inV)) emptyOut
    else {
      buildResult(inV, ref())(pull.expr(_))
    }
  }

  private def mkRef(inV: In[A])(implicit tx: T): Tuples =
    map(inV) { v =>
      it.setValue(v)
      val (f, d) = ctx.nested(it) {
        val _f = fun.expand[T]
        _f.changed ---> this
        _f
      }
      (f, d)
    }

  final def value(implicit tx: T): Out =
    IPush.tryPull(this).fold({
      val inV = in.value
      if (isEmpty(inV)) emptyOut
      else {
        buildResult(inV, ref())(_.value)
      }
    })(_.now)

  final def dispose()(implicit tx: T): Unit = {
    in .changed.-/->(this)
    disposeRef(ref.swap(emptyIn))
  }

  def changed: IChangeEvent[T, Out] = this
}

// sequence input

abstract class ExpandedMapSeqIn[T <: Txn[T], A, P, Out](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                         /* closure: Graph, */ fun: Ex[P], tx0: T)
                                                        (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqOrOption[T, A, Seq, P, Out](in, it, fun, tx0) {

  // ---- impl ----

  protected final def isEmpty[A1](in: Seq[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Seq[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def emptyIn[A1]: Seq[A1] = Nil

  protected final def map[A1, B1](in: Seq[A1])(body: A1 => B1): Seq[B1] = in.map(body)
}

abstract class ExpandedMapSeqLike[T <: Txn[T], A, P, B](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                          /* closure: Graph, */ fun: Ex[P], tx0: T)
                                                         (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqIn[T, A, P, Seq[B]](in, it, fun, tx0) {

  // ---- abstract: out ----

  protected def append(b: mutable.Builder[B, _], cc: P): Unit

  // ---- impl ----

  protected final def emptyOut: Seq[B] = Nil

  protected final def buildResult(inV: Seq[A], tuples: Tuples)(elem: IExpr[T, P] => P)
                                 (implicit tx: T): Seq[B] = {
    if (tuples.size != inV.size) {
      val err = new AssertionError(s"inV.size = ${inV.size}, tuples.size = ${tuples.size} in $this")
//      err.fillInStackTrace()
//      Console.err.println(s"Assertion failed in $this")
//      err.printStackTrace()
      throw err
    }
    val iterator  = inV.iterator zip tuples.iterator
    val b         = Seq.newBuilder[B]
    b.sizeHint(inV)
    while (iterator.hasNext) {
      val (vn, (f, _)) = iterator.next()
      it.setValue(vn /*, dispatch = true*/)
      val funV = elem(f)
      append(b, funV)
    }
    b.result()
  }
}

final class ExpandedMapSeq[T <: Txn[T], A, B](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                              /* closure: Graph, */ fun: Ex[B], tx0: T)
                                             (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqLike[T, A, B, B](in, it, fun, tx0) {

  override def toString: String = s"$in.map($fun)"

  protected def append(b: mutable.Builder[B, _], cc: B): Unit = b += cc
}

final class ExpandedFlatMapSeq[T <: Txn[T], A, B](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                  /* closure: Graph, */ fun: Ex[Seq[B]], tx0: T)
                                                 (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqLike[T, A, Seq[B], B](in, it, fun, tx0) {

  override def toString: String = s"$in.flatMap($fun)"

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapSeqOption[T <: Txn[T], A, B](in: IExpr[T, Seq[A]], it: It.Expanded[T, A],
                                                        /* closure: Graph, */ fun: Ex[Option[B]], tx0: T)
                                                       (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqLike[T, A, Option[B], B](in, it, fun, tx0) {

  override def toString: String = s"$in.flatMap($fun)"

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}

// option input

abstract class ExpandedMapOptionLike[T <: Txn[T], A, P, B](in: IExpr[T, Option[A]], it: It.Expanded[T, A],
                                                         /* closure: Graph, */ fun: Ex[P], tx0: T)
                                                        (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapSeqOrOption[T, A, Option, P, Option[B]](in, it, fun, tx0) {

  // ---- impl ----

  protected final def isEmpty[A1](in: Option[A1]): Boolean = in.isEmpty

  protected final def foreach[A1](in: Option[A1])(body: A1 => Unit): Unit = in.foreach(body)

  protected final def emptyIn[A1]: Option[A1] = None

  protected final def emptyOut: Option[B] = None

  protected final def map[A1, B1](in: Option[A1])(body: A1 => B1): Option[B1] = in.map(body)
}

final class ExpandedMapOption[T <: Txn[T], A, B](in: IExpr[T, Option[A]], it: It.Expanded[T, A],
                                                 fun: Ex[B], tx0: T)
                                                (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapOptionLike[T, A, B, B](in, it, fun, tx0) {

  override def toString: String = s"$in.map($fun)"

  protected def buildResult(inV: Option[A], tuples: Tuples)(elem: IExpr[T, B] => B)
                           (implicit tx: T): Option[B] =
    inV.flatMap { vn =>
      tuples.map { case (f, _) =>
        it.setValue(vn)
        elem(f)
      }
    }
}

final class ExpandedFlatMapOption[T <: Txn[T], A, B](in: IExpr[T, Option[A]], it: It.Expanded[T, A],
                                                     fun: Ex[Option[B]], tx0: T)
                                                    (implicit targets: ITargets[T], ctx: Context[T])
  extends ExpandedMapOptionLike[T, A, Option[B], B](in, it, fun, tx0) {

  override def toString: String = s"$in.flatMap($fun)"

  protected def buildResult(inV: Option[A], tuples: Tuples)(elem: IExpr[T, Option[B]] => Option[B])
                           (implicit tx: T): Option[B] =
    inV.flatMap { vn =>
      tuples.flatMap { case (f, _) =>
        it.setValue(vn)
        elem(f)
      }
    }
}