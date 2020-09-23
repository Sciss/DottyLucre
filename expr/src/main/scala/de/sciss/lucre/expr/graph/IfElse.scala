/*
 *  IfElse.scala
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

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}

import scala.annotation.tailrec

/** Beginning of a conditional block.
  *
  * @param cond   the condition or predicate for the `then` branch.
  *
  * @see  [[IfThen]]
  */
final case class If(cond: Ex[Boolean]) {
  def Then [A](branch: Ex[A]): IfThen[A] =
    IfThen(cond, branch)

  def Then (branch: Act): IfThenAct =
    IfThenAct(cond, branch)
}

sealed trait Then[+A] /*extends Ex[Option[A]]*/ {
  def cond  : Ex[Boolean]
  def result: Ex[A]

  import de.sciss.lucre.expr.graph.{Else => _Else}

  def Else [B >: A](branch: Ex[B]): Ex[B] =
    _Else(this, branch)

  final def ElseIf (cond: Ex[Boolean]): ElseIf[A] =
    new ElseIf(this, cond)
}

/** A side effecting conditional block. To turn it into a full `if-then-else` construction,
  * call `Else` or `ElseIf`.
  *
  * @see  [[Else]]
  * @see  [[ElseIf]]
  */
final case class IfThen[+A](cond: Ex[Boolean], result: Ex[A]) extends Then[A]

final case class ElseIf[+A](pred: Then[A], cond: Ex[Boolean]) {
  def Then [B >: A](branch: Ex[B]): ElseIfThen[B] =
    ElseIfThen[B](pred, cond, branch)
}

final case class ElseIfThen[+A](pred: Then[A], cond: Ex[Boolean], result: Ex[A])
  extends Then[A]

object Else {
  private type Case[T <: Txn[T], A] = (IExpr[T, Boolean], IExpr[T, A])

  private def gather[T <: Txn[T], A](e: Then[A])(implicit ctx: Context[T],
                                                        tx: T): List[Case[T, A]] = {
    @tailrec
    def loop(t: Then[A], res: List[Case[T, A]]): List[Case[T, A]] = {
      val condEx  = t.cond  .expand[T]
      val resEx   = t.result.expand[T]
      val res1 = (condEx, resEx) :: res
      t match {
        case hd: ElseIfThen[A] => loop(hd.pred, res1)
        case _ => res1
      }
    }

    loop(e, Nil)
  }

  private final class Expanded[T <: Txn[T], A](cases: List[Case[T, A]], default: IExpr[T, A], tx0: T)
                                              (implicit protected val targets: ITargets[T])
    extends IExpr[T, A] with IChangeEventImpl[T, A] {

    cases.foreach { case (cond, res) =>
      cond.changed.--->(this)(tx0)
      res .changed.--->(this)(tx0)
    }
    default.changed.--->(this)(tx0)

    def value(implicit tx: T): A = {
      @tailrec
      def loop(rem: List[Case[T, A]]): A = rem match {
        case (cond, branch) :: tail =>
          if (cond.value) branch.value else loop(tail)

        case Nil => default.value
      }

      loop(cases)
    }

    def changed: IChangeEvent[T, A] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      type CaseValue = (Boolean, A)

      // XXX TODO --- this evaluates all branches; we could optimize
      @tailrec
      def loop1(rem: List[Case[T, A]], res: List[CaseValue]): List[CaseValue] = rem match {
        case (cond, branch) :: tail =>
          val condV     = pull.expr(cond  )
          val branchV   = pull.expr(branch)
          loop1(tail, (condV, branchV) :: res)

        case Nil => res.reverse
      }

      val defaultV = pull.expr(default)

      @tailrec
      def loop2(rem: List[CaseValue]): A = rem match {
        case (condV, branchV) :: tail =>
          if (condV) branchV else loop2(tail)

        case Nil => defaultV
      }

      val casesCh = loop1(cases, Nil)
      loop2(casesCh)
    }

//    private[lucre] def pullUpdateXXX(pull: IPull[T])(implicit tx: T): Option[Change[A]] = {
//      type CaseChange = (Change[Boolean], Change[A])
//
//      // XXX TODO --- this evaluates all branches; we could optimize
//      @tailrec
//      def loop1(rem: List[Case[T, A]], res: List[CaseChange]): List[CaseChange] = rem match {
//        case (cond, branch) :: tail =>
//          val condEvt     = cond  .changed
//          val branchEvt   = branch.changed
//          val condChOpt   = if (pull.contains(condEvt  )) pull(condEvt   ) else None
//          val branchChOpt = if (pull.contains(branchEvt)) pull(branchEvt ) else None
//          val condCh      = condChOpt.getOrElse {
//            val condV = cond.value
//            Change(condV, condV)
//          }
//          val branchCh    = branchChOpt.getOrElse {
//            val branchV = branch.value
//            Change(branchV, branchV)
//          }
//
//          loop1(tail, (condCh, branchCh) :: res)
//
//        case Nil => res.reverse
//      }
//
//      val defaultEvt    = default.changed
//      val defaultChOpt  = if (pull.contains(defaultEvt)) pull(defaultEvt) else None
//      val defaultCh     = defaultChOpt.getOrElse {
//        val defaultV = default.value
//        Change(defaultV, defaultV)
//      }
//
//      @tailrec
//      def loop2(rem: List[CaseChange], isBefore: Boolean): A = rem match {
//        case (cond, branch) :: tail =>
//          val condV = if (isBefore) cond.before else cond.now
//          if (condV) {
//            if (isBefore) branch.before else branch.now
//          } else loop2(tail, isBefore = isBefore)
//
//        case Nil =>
//          if (isBefore) defaultCh.before else defaultCh.now
//      }
//
//      val casesCh     = loop1(cases, Nil)
//      val valueBefore = loop2(casesCh, isBefore = true  )
//      val valueNow    = loop2(casesCh, isBefore = false )
//      val valueCh     = Change(valueBefore, valueNow)
//
//      if (valueCh.isSignificant) Some(valueCh) else None
//    }

    def dispose()(implicit tx: T): Unit = {
      cases.foreach { case (cond, res) =>
        cond.changed.-/->(this)
        res .changed.-/->(this)
      }
      default.changed.-/->(this)
    }
  }
}
final case class Else[A](pred: Then[A], default: Ex[A]) extends Ex[A] {
  type Repr[T <: Txn[T]] = IExpr[T, A]

  def cond: Ex[Boolean] = true

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    val cases     = Else.gather(pred)
    val defaultEx = default.expand[T]

    import ctx.targets
    new Else.Expanded(cases, defaultEx, tx)
  }
}

// ---- Act variants ----

final case class ElseIfAct(pred: ThenAct, cond: Ex[Boolean]) {
  def Then (branch: Act): ElseIfThenAct =
    ElseIfThenAct(pred, cond, branch)
}

object ThenAct {
  private type Case[T <: Txn[T]] = (IExpr[T, Boolean], IAction[T])

  private def gather[T <: Txn[T]](e: ThenAct)(implicit ctx: Context[T],
                                                 tx: T): List[Case[T]] = {
    @tailrec
    def loop(t: ThenAct, res: List[Case[T]]): List[Case[T]] = {
      val condEx  = t.cond  .expand[T]
      val resEx   = t.result.expand[T]
      val res1 = (condEx, resEx) :: res
      t match {
        case hd: ElseIfThenAct => loop(hd.pred, res1)
        case _ => res1
      }
    }

    loop(e, Nil)
  }

  private final class Expanded[T <: Txn[T]](cases: List[Case[T]])
    extends IAction.Option[T] with IActionImpl[T] {

    def isDefined(implicit tx: T): Boolean = cases.exists(_._1.value)

    def executeAction()(implicit tx: T): Unit = executeIfDefined()

    def executeIfDefined()(implicit tx: T): Boolean = {
      @tailrec
      def loop(rem: List[Case[T]]): Boolean = rem match {
        case (hdCond, hdAct) :: tail =>
          if (hdCond.value) {
            hdAct.executeAction()
            true
          } else {
            loop(tail)
          }

        case Nil => false
      }

      loop(cases)
    }
  }
}

sealed trait ThenAct extends Act {
  type Repr[T <: Txn[T]] = IAction.Option[T]

  def cond  : Ex[Boolean]
  def result: Act

  def Else (branch: Act): Act =
    ElseAct(this, branch)

  final def ElseIf (cond: Ex[Boolean]): ElseIfAct =
    ElseIfAct(this, cond)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    val cases = ThenAct.gather(this)
    new ThenAct.Expanded(cases)
  }
}

object ElseAct {
  private final class Expanded[T <: Txn[T]](pred: IAction.Option[T], default: IAction[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      if (!pred.executeIfDefined()) default.executeAction()
  }
}
final case class ElseAct(pred: ThenAct, default: Act) extends Act {
  type Repr[T <: Txn[T]] = IAction[T]

  def cond: Ex[Boolean] = true

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    val predEx    = pred    .expand[T]
    val defaultEx = default .expand[T]
    new ElseAct.Expanded(predEx, defaultEx)
  }
}

final case class ElseIfThenAct(pred: ThenAct, cond: Ex[Boolean], result: Act) extends ThenAct

/** A side effecting conditional block. To turn it into a full `if-then-else` construction,
  * call `Else` or `ElseIf`.
  *
  * @see  [[Else]]
  * @see  [[ElseIf]]
  */
final case class IfThenAct(cond: Ex[Boolean], result: Act) extends ThenAct