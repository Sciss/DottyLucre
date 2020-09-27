/*
 *  Var.scala
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

import de.sciss.serial.Writable

trait Source[-T, +A] {
  def apply()(implicit tx: T): A
}

trait Sink[-T, -A] {
  def update(value: A)(implicit tx: T): Unit
}

trait Ref[-T, A] extends Source[T, A] with Sink[T, A] {
  def swap(value: A)(implicit tx: T): A
}

trait Var[-T, A] extends Ref[T, A] with Writable with Disposable[T]
