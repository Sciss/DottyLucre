/*
 *  Reader.scala
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

object Reader {
  type Immutable[A] = Reader[Any, Any, A]
}
trait Reader[-Tx, -Acc, +A] {
  def read(in: DataInput, access: Acc)(implicit tx: Tx): A
}
