/*
 *  ObservableImpl.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer

import scala.concurrent.stm.Ref

trait ObservableImpl[T <: Txn[T], U] extends Observable[T, U] {
  private[this] final class Observation(val fun: T => U => Unit) extends Disposable[T] {
    def dispose()(implicit tx: T): Unit = removeObservation(this)
  }

  private[this] val obsRef = Ref(Vector.empty[Observation])

  protected final def fire(update: U)(implicit tx: T): Unit = {
    val obs = obsRef()
    obs.foreach(_.fun(tx)(update))
  }

  private[this] def removeObservation(obs: Observation)(implicit tx: T): Unit =
    obsRef.transform(_.filterNot(_ === obs))

  final def react(fun: T => U => Unit)(implicit tx: T): Disposable[T] = {
    val obs = new Observation(fun)
    obsRef.transform(_ :+ obs)(tx.peer)
    obs
  }
}

trait DummyObservableImpl[T <: Txn[T]] extends Observable[T, Nothing] {
  def react(fun: T => Nothing => Unit)(implicit tx: T): Disposable[T] = Disposable.empty
}