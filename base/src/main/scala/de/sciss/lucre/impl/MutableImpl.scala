/*
 *  MutableImpl.scala
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

package de.sciss.lucre.impl

import de.sciss.lucre.{Exec, Mutable}
import de.sciss.serial.DataOutput

trait MutableImpl[T <: Exec[T]] extends Mutable[T] {
  final override def dispose(): Unit = {
    id.dispose()
    disposeData()
  }

  final override def write(out: DataOutput): Unit = {
    id.write(out)
    writeData(out)
  }

  protected def disposeData(): Unit
  protected def writeData(out: DataOutput): Unit

  override def toString = s"${super.toString}$id"
}