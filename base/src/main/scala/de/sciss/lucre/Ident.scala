/*
 *  Identifier.scala
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

import de.sciss.serial
import de.sciss.serial.{DataInput, TFormat}

trait Ident[T <: Exec[T]] extends Disposable with serial.Writable {
  
  def newVar[A](init: A)(implicit format: TFormat[T, A]): Var[A]

  def newBooleanVar(init: Boolean): Var[Boolean]
  def newIntVar    (init: Int    ): Var[Int]
  def newLongVar   (init: Long   ): Var[Long]

  def readVar[A](in: DataInput)(implicit format: TFormat[T, A]): Var[A]

  def readBooleanVar(in: DataInput): Var[Boolean]
  def readIntVar    (in: DataInput): Var[Int]
  def readLongVar   (in: DataInput): Var[Long]

  /** Ensures that the identifier is actually valid in the current transaction. */
  def ! (implicit tx: T): tx.Id
}