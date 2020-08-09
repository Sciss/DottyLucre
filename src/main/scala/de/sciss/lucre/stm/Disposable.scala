/*
 *  Disposable.scala
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

package de.sciss.lucre.stm

object Disposable {
  private object Empty extends Disposable[Any] {
    override def toString = "Disposable.empty"

    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[Tx]: Disposable[Tx] = Empty

  def seq[Tx](xs: Disposable[Tx]*): Disposable[Tx] = new Disposable[Tx] {
    def dispose()(implicit tx: Tx): Unit = xs.foreach(_.dispose())
  }
}
trait Disposable[-Tx] {
  def dispose()(implicit tx: Tx): Unit
}