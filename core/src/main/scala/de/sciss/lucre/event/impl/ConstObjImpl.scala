/*
 *  ConstObjImpl.scala
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

package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.{Obj, Txn}
import de.sciss.serial.DataOutput

trait ConstObjImpl[T <: Txn[T], A] extends Obj[T] with Publisher[T, A] {
  private[lucre] def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

  final def changed: EventLike[T, A] = Dummy[T, A]

  final def write(out: DataOutput): Unit = {
    out.writeInt(tpe.typeId)
    out.writeByte(3)
    id.write(out)
    writeData(out)
  }

  final def dispose()(implicit tx: T): Unit = id.dispose()

  protected def writeData(out: DataOutput): Unit
}