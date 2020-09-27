/*
 *  InMemoryIdMapImpl.scala
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

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.{Ident, IdentMap}

import scala.concurrent.stm.TxnLocal

private[impl] final class InMemoryIdMapImpl[T <: Txn[T], A](val store: InMemoryConfluentMap[T, Int])
  extends IdentMap[T, A] with InMemoryCacheMapImpl[T, Int] {

  private val markDirtyFlag = TxnLocal(false)

  private def markDirty()(implicit tx: T): Unit =
    if (!markDirtyFlag.swap(true)(tx.peer)) {
      tx.addDirtyLocalCache(this)
    }

  def get(id: Ident[T])(implicit tx: T): Option[A] = {
    val idc = id.!
    getCache[A](idc.base, tx)(idc.path)
  }

  def getOrElse(id: Ident[T], default: => A)(implicit tx: T): A =
    get(id).getOrElse(default)

  def put(id: Ident[T], value: A)(implicit tx: T): Unit = {
    val idc = id.!
    putCache[A](idc.base, value, tx)(idc.path)
    markDirty()
  }

  def contains(id: Ident[T])(implicit tx: T): Boolean =
    get(id).isDefined // XXX TODO more efficient implementation

  def remove(id: Ident[T])(implicit tx: T): Unit = {
    val idc = id.!
    if (removeCache(idc.base, tx)(idc.path)) markDirty()
  }

  def dispose()(implicit tx: T): Unit = ()

  override def toString = s"IdentifierMap@${hashCode().toHexString}"
}