/*
 *  SysInMemoryRef.scala
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

import de.sciss.serial.DataOutput

import scala.concurrent.stm.{Ref => ScalaRef}

final class SysInMemoryRef[A](val peer: ScalaRef[A])
  extends InMemoryLike.Var[TxnLike, A] {

  type T = TxnLike

  import Txn.{peer => itx}

  override def toString = s"Var<${hashCode().toHexString}>"

  def apply()     (implicit tx: T): A    = peer.get
  def update(v: A)(implicit tx: T): Unit = peer.set (v)
  def swap  (v: A)(implicit tx: T): A    = peer.swap(v)

  def write(out: DataOutput): Unit = ()

  def dispose()(implicit tx: T): Unit =
    peer.set(null.asInstanceOf[A])
}
