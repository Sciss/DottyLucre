/*
 *  ImmutableReader.scala
 *  (Serial)
 *
 * Copyright (c) 2011-2020 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss
package serial

trait ImmutableReader[+A] extends Reader[Any, Any, A] {
  def read(in: DataInput): A

  def read(in: DataInput, access: Any)(implicit tx: Any): A = read(in)
}
