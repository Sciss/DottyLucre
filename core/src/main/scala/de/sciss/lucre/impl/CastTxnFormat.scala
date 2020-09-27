/*
 *  CastTxnFormat.scala
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

package de.sciss.lucre.impl

import de.sciss.lucre.Txn
import de.sciss.serial.{TFormat, Writable, WritableFormat}

trait CastTxnFormat[T <: Txn[T], Repr[~ <: Txn[~]] <: Writable] extends WritableFormat[T, Repr[T]] {
  def cast[T1 <: Txn[T1]]: TFormat[T1, Repr[T1]] = this.asInstanceOf[TFormat[T1, Repr[T1]]]
}
