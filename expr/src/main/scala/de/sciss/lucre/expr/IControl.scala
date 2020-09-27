/*
 *  IControl.scala
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

package de.sciss.lucre
package expr

object IControl {
  def wrap[T <: Exec[T]](peer: Disposable[T]): IControl[T] = new Wrap(peer)

  def empty[T <: Exec[T]]: IControl[T] = new Empty

  private final class Wrap[T <: Exec[T]](peer: Disposable[T]) extends IControl[T] {
    def initControl()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = peer.dispose()
  }

  private final class Empty[T <: Exec[T]] extends IControl[T] {
    def initControl()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = ()
  }
}
trait IControl[T <: Exec[T]] extends Form[T] with Disposable[T] {
  def initControl()(implicit tx: T): Unit
}