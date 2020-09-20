/*
 *  VarImpl.scala
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

import de.sciss.lucre
import de.sciss.model.Change
import de.sciss.serial.DataOutput

trait ExprVarImpl[T <: Txn[T], A, Repr <: Expr[T, A]]
  extends Expr[T, A] with ExprNodeImpl[T, A] with lucre.Var[Repr] { self =>
  
  protected def tx: T

  // ---- abstract ----

  protected def ref: Var[Repr]

  // ---- implemented ----

  object changed extends Changed with GeneratorEvent[T, Change[A]] {
    private[lucre] def pullUpdate(pull: Pull[T]): Option[Change[A]] =
      if (pull.parents(this).isEmpty) {
        Some(pull.resolve[Change[A]])
      } else {
        pull(self().changed)
      }
  }

  final protected def writeData(out: DataOutput): Unit = {
    out.writeByte(0)
    ref.write(out)
  }

  final protected def disposeData(): Unit = {
    disconnect()
    ref.dispose()
  }

  final def connect(): this.type = {
    ref().changed ---> changed
    this
  }

  private[this] def disconnect(): Unit = ref().changed -/-> changed

  final def apply(): Repr = ref()

  final def update(expr: Repr): Unit = {
    val before = ref()
    if (before != expr) {
      before.changed -/-> this.changed
      ref() = expr
      expr  .changed ---> this.changed

      val beforeV = before.value
      val exprV   = expr  .value
      changed.fire(Change(beforeV, exprV))(tx)
    }
  }

  final def swap(expr: Repr)(implicit tx: T): Repr = {
    val res = apply()
    update(expr)
    res
  }

  final def value: A = ref().value

  override def toString = s"Expr.Var$id"
}