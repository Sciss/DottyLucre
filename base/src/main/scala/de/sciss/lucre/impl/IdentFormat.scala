/*
 *  IdentifierSerializer.scala
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

import de.sciss.serial.DataInput

final class IdentFormat[T <: Exec[T]] extends CastExecFormat[T, Ident] {
  def readT(in: DataInput)(implicit tx: T): Ident[T] = tx.readId(in)
}
