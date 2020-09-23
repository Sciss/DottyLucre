/*
 *  Tuples.scala
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
package impl

import de.sciss.model.Change
import de.sciss.serial.DataOutput

trait ExprTuple1Op[A, T1, ReprA[~ <: Txn[~]] <: Expr[~, A], ReprT1[~ <: Txn[~]] <: Expr[~, T1]] {
  def id: Int

  def value(a: T1): A

  final def unapply[T <: Txn[T]](ex: ReprA[T])/* (implicit tx: T) */: Option[ReprT1[T]] = ex match {
    case tup0: ExprTuple1[_, _, _, _, _] if tup0.op == this =>
      val tup = tup0.asInstanceOf[ExprTuple1[T, A, T1, ReprA, ReprT1]]
      Some(tup._1)
    case _ => None
  }

  def toString[T <: Txn[T]](_1: ReprT1[T]): String
}

trait ExprTuple1[T <: Txn[T], A, T1, ReprA[~ <: Txn[~]] <: Expr[~, A], ReprT1[~ <: Txn[~]] <: Expr[~, T1]]
  extends ExprNodeImpl[T, A] {
  _: ReprA[T] =>

  def op: ExprTuple1Op[A, T1, ReprA, ReprT1]
  def _1: ReprT1[T]

  def connect()(implicit tx: T): this.type = {
    _1.changed ---> changed
    this
  }

  private[this] def disconnect()(implicit tx: T): Unit = _1.changed -/-> changed

  protected def disposeData()(implicit tx: T): Unit = disconnect()

  def value(implicit tx: T): A = op.value(_1.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(1)  // 'node not var'
    // out.writeInt(typeId)
    out.writeInt(op.id)
    _1.write(out)
  }

  object changed extends Changed {
    private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[A]] =
      pull(_1.changed).flatMap { ach =>
        val before  = op.value(ach.before)
        val now     = op.value(ach.now   )
        if (before == now) None else Some(Change(before, now))
      }
  }

  override def toString: String = op.toString(_1)
}

trait ExprTuple2Op[A, T1, T2, ReprA[~ <: Txn[~]] <: Expr[~, A], ReprT1[~ <: Txn[~]] <: Expr[~, T1],
  ReprT2[~ <: Txn[~]] <: Expr[~, T2]] {

  def id: Int

  def value(a: T1, b: T2): A

  // final protected def writeTypes(out: DataOutput) = ()

  final def unapply[T <: Txn[T]](ex: ReprA[T])/* (implicit tx: T) */: Option[(ReprT1[T], ReprT2[T])] =
    ex match {
      case tup0: ExprTuple2[_, _, _, _, _, _, _] if tup0.op == this =>
        val tup = tup0.asInstanceOf[ExprTuple2[T, A, T1, T2, ReprA, ReprT1, ReprT2]]
        Some((tup._1, tup._2))
      case _ => None
    }

  def toString[T <: Txn[T]](_1: ReprT1[T], _2: ReprT2[T]): String
}

trait ExprTuple2[T <: Txn[T], A, T1, T2, ReprA[~ <: Txn[~]] <: Expr[~, A], ReprT1[~ <: Txn[~]] <: Expr[~, T1],
  ReprT2[~ <: Txn[~]] <: Expr[~, T2]]
  extends ExprNodeImpl[T, A] {

  this: ReprA[T] =>

  def op: ExprTuple2Op[A, T1, T2, ReprA, ReprT1, ReprT2]
  def _1: ReprT1[T]
  def _2: ReprT2[T]

  def connect()(implicit tx: T): this.type = {
    _1.changed ---> changed
    _2.changed ---> changed
    this
  }

  private[this] def disconnect()(implicit tx: T): Unit = {
    _1.changed -/-> changed
    _2.changed -/-> changed
  }

  protected def disposeData()(implicit tx: T): Unit = disconnect()

  def value(implicit tx: T): A = op.value(_1.value, _2.value)

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(1)  // 'node not var'
    //    out.writeByte(2)
    //    out.writeInt(typeId)
    out.writeInt(op.id)
    _1.write(out)
    _2.write(out)
  }

  object changed extends Changed {
    private[lucre] def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[A]] = {
      val _1c = _1.changed
      val _2c = _2.changed

      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
      val _2ch = if (pull.contains(_2c)) pull(_2c) else None

      (_1ch, _2ch) match {
        case (Some(ach), None) =>
          val bv      = _2.value
          val before  = op.value(ach.before, bv)
          val now     = op.value(ach.now, bv)
          if (before == now) None else Some(Change(before, now))
        case (None, Some(bch)) =>
          val av      = _1.value
          val before  = op.value(av, bch.before)
          val now     = op.value(av, bch.now)
          if (before == now) None else Some(Change(before, now))
        case (Some(ach), Some(bch)) =>
          val before  = op.value(ach.before, bch.before)
          val now     = op.value(ach.now, bch.now)
          if (before == now) None else Some(Change(before, now))
        case _ => None
      }
    }
  }

  override def toString: String = op.toString(_1, _2)
}