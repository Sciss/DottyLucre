package de.sciss.lucre.impl

import de.sciss.lucre.Exec
import de.sciss.serial.{TFormat, Writable, WritableFormat}

trait CastExecFormat[T <: Exec[T], Repr[~ <: Exec[~]] <: Writable] extends WritableFormat[T, Repr[T]] {
  def cast[T1 <: Exec[T1]]: TFormat[T1, Repr[T1]] = this.asInstanceOf[TFormat[T1, Repr[T1]]]
}
