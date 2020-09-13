/*
 *  Copy.scala
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

package de.sciss.lucre.experiment

import de.sciss.lucre.experiment.impl.CopyImpl

import scala.reflect.ClassTag

object Copy {
  def apply[In <: Txn[In], Out <: Txn[Out]]()(implicit txIn: In, txOut: Out): Copy[In, Out] =
    new CopyImpl[In, Out]
}
trait Copy[In <: Txn[In], Out <: Txn[Out]] {
  /** Makes a deep copy of the input element. Passing in an `Obj`
   * will also copy the attributes.
   */
  def apply[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out]

  /** Provides a hint for the input element with key and value
   * by convention.
   */
  def putHint[A](in: Elem[In], key: String, value: A): Unit

  def getHint[A](in: Elem[In], key: String)(implicit ct: ClassTag[A]): Option[A]

  /** Copies all attributes from input to output. */
  def copyAttr(in: Obj[In], out: Obj[Out]): Unit

  /** Stores an early copy of an object from within its own `copy` method,
   * to allow mutual correspondences. The copy is only completed with
   * the provided `code` thunk argument which will be executed in the
   * `finish()` stage.
   */
  def defer[Repr[~ <: Txn[~]] <: Obj[~]](in: Repr[In], out: Repr[Out])(code: => Unit): Unit

  def finish(): Unit

  /** Copies the object but not its attributes. */
  def copyPlain[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out]
}