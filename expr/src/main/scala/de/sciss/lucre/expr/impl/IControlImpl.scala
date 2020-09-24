/*
 *  IControlImpl.scala
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

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.expr.graph.{Control, Ex}
import de.sciss.lucre.{Disposable, Txn}

trait IControlImpl[T <: Txn[T]] extends IControl[T] {
  private[this] var obs = List.empty[Disposable[T]]

  protected def peer: Control

  /** The default does nothing. Sub-classes may override this. */
  def initControl()(implicit tx: T): Unit = ()

  protected final def initProperty[A](key: String, default: A)(set: T => A => Unit)
                                     (implicit tx: T, ctx: Context[T]): Unit =
    ctx.getProperty[Ex[A]](peer, key) match {
      case Some(ex) =>
        val expr    = ex.expand[T]
        val value0  = expr.value
        if (value0 != default) {
          set(tx)(value0)
        }
        obs ::= expr.changed.react { implicit tx => upd =>
          set(tx)(upd.now)
        }

      case _ =>
    }

  def dispose()(implicit tx: T): Unit =
    obs.foreach(_.dispose())
}
