/*
 *  File.scala
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

import java.io.{File => _File}

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object File {
  private final class MkDirExpanded[T <: Txn[T]](f: IExpr[T, _File])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val fv = f.value
      tx.afterCommit(fv.mkdirs()) // plural! we want to ensure parent directories are created, too
    }
  }

  final case class MkDir(f: Ex[_File]) extends Act {
    override def productPrefix: String = s"File$$MkDir"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new MkDirExpanded(f.expand[T])
  }

  private final class DeleteExpanded[T <: Txn[T]](f: IExpr[T, _File])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val fv = f.value
      tx.afterCommit(fv.delete())
    }
  }

  final case class Delete(f: Ex[_File]) extends Act {
    override def productPrefix: String = s"File$$Delete"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new DeleteExpanded(f.expand[T])
  }

  private final class ListExpanded[T <: Txn[T]](dir: IExpr[T, _File])(implicit protected val targets: ITargets[T])
    extends IExpr[T, Seq[_File]] with IActionImpl[T] with IChangeGeneratorEvent[T, Seq[_File]] {

    private[this] val ref = Ref(Seq.empty[_File])

    def value(implicit tx: T): Seq[_File] = ref()

    override def changed: IChangeEvent[T, Seq[_File]] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Seq[_File] =
      pull.resolveExpr(this)

    def executeAction()(implicit tx: T): Unit = {
      val dv      = dir.value
      val arr     = dv.listFiles()
      val now     = if (arr == null) Nil else arr.toSeq
      val before  = ref.swap(now)
      if (before !== now) {
        fire(Change(before, now))
      }
    }
  }

  final case class List(dir: Ex[_File]) extends Ex[Seq[_File]] with Act {
    override def productPrefix: String = s"File$$List"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Seq[_File]] with IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ListExpanded(dir.expand[T])
    }
  }
}
