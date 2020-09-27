/*
 *  Ident.scala
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

import de.sciss.lucre.impl.IdentFormat
import de.sciss.serial
import de.sciss.serial.{DataInput, TFormat}

object Ident {
  implicit def format[T <: Exec[T]]: TFormat[T, Ident[T]] = anyFmt.cast

  private val anyFmt = new IdentFormat[AnyExec]
}
trait Ident[T <: Exec[T]] extends Disposable[T] with serial.Writable {
  
  def newVar[A](init: A)(implicit tx: T, format: TFormat[T, A]): Var[T, A]

  def newBooleanVar(init: Boolean)(implicit tx: T): Var[T, Boolean]
  def newIntVar    (init: Int    )(implicit tx: T): Var[T, Int]
  def newLongVar   (init: Long   )(implicit tx: T): Var[T, Long]

  def readVar[A](in: DataInput)(implicit format: TFormat[T, A]): Var[T, A]

  def readBooleanVar(in: DataInput): Var[T, Boolean]
  def readIntVar    (in: DataInput): Var[T, Int]
  def readLongVar   (in: DataInput): Var[T, Long]

  /** Ensures that the identifier is actually valid in the current transaction. */
  def ! (implicit tx: T): tx.Id
}