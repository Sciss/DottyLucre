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

package de.sciss.lucre.experiment.impl

import de.sciss.lucre.experiment.{InMemoryLike, Txn}
import de.sciss.serial.DataOutput

import scala.concurrent.stm.{InTxn, Ref => ScalaRef}

final class SysInMemoryRef[A](val peer: ScalaRef[A], itx: InTxn)
  extends InMemoryLike.Var[A] {

  override def toString = s"Var<${hashCode().toHexString}>"

  def apply()     : A    = peer.get    (itx)
  def update(v: A): Unit = peer.set (v)(itx)
  def swap  (v: A): A    = peer.swap(v)(itx)

  def write(out: DataOutput): Unit = ()

  def dispose(): Unit =
    peer.set(null.asInstanceOf[A])(itx)
}
