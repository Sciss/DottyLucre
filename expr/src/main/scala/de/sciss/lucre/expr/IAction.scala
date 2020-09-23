/*
 *  IAction.scala
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

object IAction {
  trait Option[T <: Exec[T]] extends IAction[T] {
    def isDefined(implicit tx: T): Boolean

    def executeIfDefined()(implicit tx: T): Boolean
  }

  def empty[T <: Exec[T]]: IAction[T] = new Empty

  private final class Empty[T <: Exec[T]] extends IAction[T] {
    def addSource(tr: ITrigger[T])(implicit tx: T): Unit = ()

    def executeAction()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = ()
  }
}
trait IAction[T <: Exec[T]] extends Form[T] with Disposable[T] {
  /** Directly adds a trigger input to the action.
   * Actions that do not produce successive events can
   * simply rewrite this as
   *
   * {{{
   * tr.changed.react { implicit tx => _ => executeAction() }
   * }}}
   *
   * If the action produces successive events, it should
   * prevent this indirection, as triggered cannot be logically
   * combined that way.
   */
  def addSource(tr: ITrigger[T])(implicit tx: T): Unit

  def executeAction()(implicit tx: T): Unit
}
