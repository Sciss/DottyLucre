package de.sciss.lucre
package impl

import de.sciss.serial.DataOutput

trait ConstElemImpl[T <: Txn[T]] extends Elem[T] {
  private[lucre] def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

  final def write(out: DataOutput): Unit = {
    out.writeInt(tpe.typeId)
    out.writeByte(3)
    writeData(out)
  }

  final def dispose()(implicit tx: T): Unit = ()

  protected def writeData(out: DataOutput): Unit
}