/*
 *  Disposable.scala
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

object Disposable {
  private object Empty extends Disposable[Any] {
    override def toString = "TDisposable.empty"

    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[T]: Disposable[T] = Empty

  def seq[T](xs: Disposable[T]*): Disposable[T] = new Disposable[T] {
    def dispose()(implicit tx: T): Unit = xs.foreach(_.dispose())
  }
}
trait Disposable[-T] {
  def dispose()(implicit tx: T): Unit
}
