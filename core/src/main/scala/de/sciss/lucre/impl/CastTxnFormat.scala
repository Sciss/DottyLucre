package de.sciss.lucre.impl

import de.sciss.lucre.Txn
import de.sciss.serial.{TFormat, Writable, WritableFormat}

trait CastTxnFormat[T <: Txn[T], Repr[~ <: Txn[~]] <: Writable] extends WritableFormat[T, Repr[T]] {
  def cast[T1 <: Txn[T1]]: TFormat[T1, Repr[T1]] = this.asInstanceOf[TFormat[T1, Repr[T1]]]
}
