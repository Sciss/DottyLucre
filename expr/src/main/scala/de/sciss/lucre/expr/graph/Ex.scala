/*
 *  Ex.scala
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

import de.sciss.equal.Implicits._
import de.sciss.file.{File => _File}
import de.sciss.lucre.Adjunct.{FromAny, HasDefault, Ord, Scalar, ScalarOrd}
import de.sciss.lucre.{Adjunct, IExpr, Txn}
import de.sciss.lucre.expr.graph.impl.{ExpandedFlatMapOption, ExpandedFlatMapSeq, ExpandedFlatMapSeqOption, ExpandedMapOption, ExpandedMapOptionAct, ExpandedMapSeq, ExpandedMapSeqAct}
import de.sciss.lucre.expr.{Context, ExBooleanOps, ExFileOps, ExOps, ExOptionOps, ExSeq, ExSeqOps, ExSpanOps, ExStringOps, ExTuple2, ExTuple2Ops, Graph, IAction}
import de.sciss.serial.DataInput
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.language.implicitConversions

object Ex {
  // ---- implicits ----

  implicit def const[A: Value](x: A): Ex[A] = Const(x)

  // ---- extractors ----

  // This doesn't work, you can only get
  // `zipped.map { case Ex(a, b) => ... }`
  // So let's leave that until we know this can be
  // expanded to multiple arities

//  def unapply[A, B](in: Ex[(A, B)]): Option[(Ex[A], Ex[B])] = Some((in._1, in._2))

  // ----

  object Value {
    implicit object anyVal    extends Value[AnyVal    ]
    implicit object string    extends Value[String    ]
    implicit object file      extends Value[_File]
    implicit object spanLike  extends Value[_SpanLike ]
//    implicit object act       extends Value[Act       ]

    implicit def tuple2 [A: Value, B: Value]: Value[(A, B)] = null

    implicit def option [A: Value]: Value[Option[A]] = null
    implicit def seq    [A: Value]: Value[Seq   [A]] = null
  }
  trait Value[-A]

  // ---- implicit lifting and ops ----

  implicit def liftTuple2_1[A, B: Value](x: (Ex[A], B)): Ex[(A, B)] = ExTuple2(x._1, Const(x._2))
  implicit def liftTuple2_2[A: Value, B](x: (A, Ex[B])): Ex[(A, B)] = ExTuple2(Const(x._1), x._2)

  implicit def liftOptionEx[A](x: Option[Ex[A]]): Ex[Option[A]] = x match {
    case Some(ex) => UnaryOp(UnaryOp.OptionSome[A](), ex)
    case None     => Const(Option.empty[A])
  }

  implicit def liftSeqEx[A](x: Seq[Ex[A]]): Ex[Seq[A]] =
    if (x.isEmpty) Const(Nil) else ExSeq(x: _*) // immutable(x): _*)

  implicit def ops      [A]   (x: Ex[A])        : ExOps       [A]     = new ExOps       (x)
  implicit def seqOps   [A]   (x: Ex[Seq  [A]]) : ExSeqOps    [A]     = new ExSeqOps    (x)
  implicit def optionOps[A]   (x: Ex[Option[A]]): ExOptionOps [A]     = new ExOptionOps (x)
  implicit def tuple2Ops[A, B](x: Ex[(A, B)])   : ExTuple2Ops [A, B]  = new ExTuple2Ops (x)
  implicit def booleanOps     (x: Ex[Boolean])  : ExBooleanOps        = new ExBooleanOps(x)
  implicit def stringOps      (x: Ex[String])   : ExStringOps         = new ExStringOps (x)
  implicit def spanOps[A <: _SpanLike](x: Ex[A]): ExSpanOps   [A]     = new ExSpanOps   (x)
  implicit def fileOps        (x: Ex[_File])    : ExFileOps           = new ExFileOps   (x)

  //////////////////////////////

  private val anyCanMapOption         = new CanMapOption        [Any]
  private val anyCanMapSeq            = new CanMapSeq           [Any]
  private val anyCanFlatMapOption     = new CanFlatMapOption    [Any]
  private val anyCanFlatMapSeq        = new CanFlatMapSeq       [Any]
  private val anyCanFlatMapSeqOption  = new CanFlatMapSeqOption [Any]

  private lazy val _init: Unit = {
    Adjunct.addFactory(anyCanMapOption        )
    Adjunct.addFactory(anyCanMapSeq           )
    Adjunct.addFactory(anyCanFlatMapOption    )
    Adjunct.addFactory(anyCanFlatMapSeq       )
    Adjunct.addFactory(anyCanFlatMapSeqOption )
    Adjunct.addFactory(CanMapOptionToAct      )
    Adjunct.addFactory(CanMapSeqToAct         )
    Adjunct.addFactory(CanFlatMapOptionToAct  )
    Adjunct.addFactory(CanFlatMapSeqToAct     )
    Adjunct.addFactory(SpanLikeTop            )
    Adjunct.addFactory(SpanTop                )
    Adjunct.addFactory(FileTop                )
  }

  def init(): Unit = _init

  final case class MapExOption[A, B] private (in: Ex[Option[A]], it: It[A], fun: Ex[B])
    extends Ex[Option[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[B]]

    override def productPrefix: String = s"Ex$$MapExOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedMapOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  final case class MapExSeq[A, B] private (in: Ex[Seq[A]], it: It[A], fun: Ex[B])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$MapExSeq" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedMapSeq[T, A, B](inEx, itEx, fun, tx)
    }
  }

  final case class MapActOption[A] private (in: Ex[Option[A]], it: It[A], fun: Act)
    extends Act.Option {

    type Repr[T <: Txn[T]] = IAction.Option[T]

    override def productPrefix: String = s"Ex$$MapActOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ExpandedMapOptionAct[T, A](inEx, itEx, fun, tx)
    }
  }

  final case class MapSeqAct[A] private (in: Ex[Seq[A]], it: It[A], fun: Act) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Ex$$MapSeqAct" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ExpandedMapSeqAct[T, A](inEx, itEx, fun, tx)
    }
  }

  final case class FlatMapExOption[A, B] private(in: Ex[Option[A]], it: It[A], fun: Ex[Option[B]])
    extends Ex[Option[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Option[B]]

    override def productPrefix: String = s"Ex$$FlatMapExOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx = in.expand[T]
      val itEx = it.expand[T]
      import ctx.targets
      new ExpandedFlatMapOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  final case class FlatMapExSeq[A, B] private (in: Ex[Seq[A]], it: It[A], fun: Ex[Seq[B]])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$FlatMapExSeq" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in  .expand[T]
      val itEx  = it  .expand[T]
      import ctx.targets
      new ExpandedFlatMapSeq[T, A, B](inEx, itEx, fun, tx)
    }
  }

  final case class FlatMapExSeqOption[A, B] private (in: Ex[Seq[A]], it: It[A], fun: Ex[Option[B]])
    extends Ex[Seq[B]] {

    type Repr[T <: Txn[T]] = IExpr[T, Seq[B]]

    override def productPrefix: String = s"Ex$$FlatMapExSeqOption" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val inEx  = in.expand[T]
      val itEx  = it.expand[T]
      import ctx.targets
      new ExpandedFlatMapSeqOption[T, A, B](inEx, itEx, fun, tx)
    }
  }

  object CanMap {
    /** One can map over an option expression */
    implicit def option[B]  : CanMap[Option , Ex[B] , Ex[Option[B]]]  = anyCanMapOption .asInstanceOf[CanMapOption  [B]]
    /** One can map over a sequence expression */
    implicit def seq   [B]  : CanMap[Seq    , Ex[B] , Ex[Seq   [B]]]  = anyCanMapSeq    .asInstanceOf[CanMapSeq     [B]]
    /** One can map over an optional action */
    implicit def optionToAct: CanMap[Option , Act   , Act.Option]     = CanMapOptionToAct
    /** One can map from a sequence expression to an action  */
    implicit def seqToAct   : CanMap[Seq    , Act   , Act]            = CanMapSeqToAct
  }
  trait CanMap[-From[_], -B, +To] extends Adjunct {
    def map[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  object CanFlatMap {
    implicit def option   [B] : CanFlatMap[Option , Ex[Option [B]], Ex[Option [B]]] = anyCanFlatMapOption.asInstanceOf[CanFlatMapOption [B]]
    implicit def seq      [B] : CanFlatMap[Seq    , Ex[Seq    [B]], Ex[Seq    [B]]] = anyCanFlatMapSeq   .asInstanceOf[CanFlatMapSeq    [B]]
    implicit def seqOption[B] : CanFlatMap[Seq    , Ex[Option [B]], Ex[Seq    [B]]] = anyCanFlatMapSeqOption.asInstanceOf[CanFlatMapSeqOption[B]]
    implicit def optionToAct  : CanFlatMap[Option , Act           , Act.Option]     = CanFlatMapOptionToAct
    implicit def seqToAct     : CanFlatMap[Seq    , Act           , Act]            = CanFlatMapSeqToAct
  }
  trait CanFlatMap[-From[_], -B, +To] {
    def flatMap[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  // --------------------- impl ---------------------

  private[expr] def mkClosure[A, B](fun: Ex[A] => B): (It[A], B) = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
//    val (c, r) = Graph.withResult {
//      fun(it)
//    }
//    (it, c, r)
    val r = fun(it)
    (it, r)
  }

  // common to all type classes
  private abstract class MapSupport extends Adjunct with Adjunct.Factory {
    def readIdentifiedAdjunct(in: DataInput): Adjunct = this
  }

  private final class CanMapOption[B] extends MapSupport
    with CanMap[Option, Ex[B], Ex[Option[B]]] {

    final val id = 1001

    override def toString = "CanMapOption"

    def map[A](from: Ex[Option[A]], fun: Ex[A] => Ex[B]): Ex[Option[B]] = {
      val (it, res) = mkClosure(fun)
      MapExOption[A, B](in = from, it = it, fun = res)
    }
  }

  private final class CanMapSeq[B] extends MapSupport
    with CanMap[Seq, Ex[B], Ex[Seq[B]]] {

    final val id = 1002

    override def toString = "CanMapSeq"

    def map[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[B]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      MapExSeq[A, B](in = from, it = it, fun = res)
    }
  }

  private final object CanMapOptionToAct extends MapSupport
    with CanMap[Option, Act, Act.Option] {

    final val id = 1003

    override def toString = "CanMapOptionToAct"

    def map[A](from: Ex[Option[A]], fun: Ex[A] => Act): Act.Option = {
      val (it, res) = mkClosure(fun)
      MapActOption[A](in = from, it = it, fun = res)
    }
  }

  private final object CanMapSeqToAct extends MapSupport
    with CanMap[Seq, Act, Act] {

    final val id = 1009

    override def toString = "CanMapSeqToAct"

    def map[A](from: Ex[Seq[A]], fun: Ex[A] => Act): Act = {
      val (it, res) = mkClosure(fun)
      MapSeqAct[A](in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapOption[B] extends MapSupport
    with CanFlatMap[Option, Ex[Option[B]], Ex[Option[B]]] {

    final val id = 1004

    override def toString = "CanFlatMapOption"

    def flatMap[A](from: Ex[Option[A]], fun: Ex[A] => Ex[Option[B]]): Ex[Option[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExOption(in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapSeq[B] extends MapSupport
    with CanFlatMap[Seq, Ex[Seq[B]], Ex[Seq[B]]] {

    final val id = 1012

    override def toString = "CanFlatMapSeq"

    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[Seq[B]]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExSeq[A, B](in = from, it = it, fun = res)
    }
  }

  private final class CanFlatMapSeqOption[B] extends MapSupport
    with CanFlatMap[Seq, Ex[Option[B]], Ex[Seq[B]]] {

    final val id = 1014

    override def toString = "CanFlatMapSeqOption"

    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Ex[Option[B]]): Ex[Seq[B]] = {
      val (it, res) = mkClosure(fun)
      FlatMapExSeqOption[A, B](in = from, it = it, fun = res)
    }
  }

  private final object CanFlatMapOptionToAct extends MapSupport
    with CanFlatMap[Option, Act, Act.Option] {

    final val id = 1011

    override def toString = "CanFlatMapOptionToAct"

    def flatMap[A](from: Ex[Option[A]], fun: Ex[A] => Act): Act.Option = {
      val (it, res) = mkClosure(fun)
      MapActOption(in = from, it = it, fun = res)
    }
  }

  private final object CanFlatMapSeqToAct extends MapSupport
    with CanFlatMap[Seq, Act, Act] {

    final val id = 1013

    override def toString = "CanFlatMapSeqToAct"

    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Act): Act = {
      val (it, res) = mkClosure(fun)
      MapSeqAct[A](in = from, it = it, fun = res)
    }
  }

  // ---- further adjuncts ----

  def spanLikeTop : FromAny[_SpanLike ] with HasDefault[_SpanLike]                        = SpanLikeTop
  def spanTop     : FromAny[_Span     ] with HasDefault[_Span    ]                        = SpanTop
  def fileTop     : FromAny[_File     ] with HasDefault[_File    ] with ScalarOrd[_File]  = FileTop

  private object SpanLikeTop extends FromAny[_SpanLike] with HasDefault[_SpanLike] with Adjunct.Factory {
    final val id = 1007

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def defaultValue: _SpanLike = _Span.Void

    def fromAny(in: Any): Option[_SpanLike] = in match {
      case s: _SpanLike => Some(s)
      case _            => None
    }
  }

  private object SpanTop extends FromAny[_Span] with HasDefault[_Span] with Adjunct.Factory {
    final val id = 1008

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def defaultValue: _Span = _Span(0L, 0L) // by convention

    def fromAny(in: Any): Option[_Span] = in match {
      case s: _Span     => Some(s)
      case _            => None
    }
  }

  private object FileTop extends FromAny[_File] with HasDefault[_File] with Ord[_File] with Scalar[_File]
    with Adjunct.Factory {

    final val id = 1015

    def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def defaultValue: _File = new _File("") // by convention

    def fromAny(in: Any): Option[_File] = in match {
      case v: _File     => Some(v)
      case _            => None
    }

    def lt  (a: _File, b: _File): Boolean = _File.NameOrdering.lt(a, b)
    def lteq(a: _File, b: _File): Boolean = _File.NameOrdering.lteq(a, b)
    def gt  (a: _File, b: _File): Boolean = _File.NameOrdering.gt(a, b)
    def gteq(a: _File, b: _File): Boolean = _File.NameOrdering.gteq(a, b)
    def eq  (a: _File, b: _File): Boolean = _File.NameOrdering.compare(a, b) === 0
    def neq (a: _File, b: _File): Boolean = _File.NameOrdering.compare(a, b) !== 0
  }
}
trait Ex[+A] extends Lazy {
  type Repr[T <: Txn[T]] <: IExpr[T, A]
}
