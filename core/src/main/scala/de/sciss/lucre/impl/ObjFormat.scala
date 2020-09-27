/*
 *  ObjFormat.scala
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
package impl

import de.sciss.equal.Implicits._
import de.sciss.serial.{DataInput, WritableFormat}

trait ObjFormat[T <: Txn[T], Repr <: Obj[T]]
  extends WritableFormat[T, Repr] {

  protected def tpe: Obj.Type

  override final def readT(in: DataInput)(implicit tx: T): Repr = {
    val tpe0 = in.readInt()
    if (tpe0 !== tpe.typeId) sys.error(s"Type mismatch, expected ${tpe.typeId}, found $tpe0")
    tpe.readIdentifiedObj(in).asInstanceOf[Repr]
  }
}

trait ObjCastFormat[T <: Txn[T], Repr[~ <: Txn[~]] <: Obj[~]]
  extends CastTxnFormat[T, Repr] with ObjFormat[T, Repr[T]]