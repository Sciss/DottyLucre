/*
 *  Txn.scala
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

import de.sciss.lucre
import de.sciss.lucre.{ConfluentLike, DurableLike}
import de.sciss.serial.{ConstFormat, TFormat}

trait Txn[T <: Txn[T]] extends lucre.Txn[T] {
  def system: ConfluentLike[T]

  type D <: DurableLike.Txn[D]

  implicit def durable: D

  implicit def durableBridge: T => D

  type Id     = Ident[T]
  type Acc    = Access[T]
  type Var[A] = lucre.Var[T, A] // confluent.Var[A]

  def inputAccess: Acc

  private[confluent] def readAccess: Acc

  /** Temporarily sets the `readAccess` while executing the body */
  private[confluent] def withReadAccess[A](path: Acc)(body: => A): A

  def info: VersionInfo.Modifiable

  /** The returned source is confluent. */
  override def newHandle[A](value: A)(implicit format: TFormat[T, A]): Source[T, A]

  def isRetroactive: Boolean

  /** The confluent handle is enhanced with the `meld` method. */
  def newHandleM[A](value: A)(implicit format: TFormat[T, A]): Source[T, A]

  private[confluent] def readTreeVertexLevel(term: Long): Int
  private[confluent] def addInputVersion(path: Acc): Unit

  private[confluent] def putTxn   [A](id: Id, value: A)(implicit format: TFormat[T, A]): Unit
  private[confluent] def putNonTxn[A](id: Id, value: A)(implicit format: ConstFormat[A]): Unit

  private[confluent] def getTxn   [A](id: Id)(implicit format: TFormat[T, A]): A
  private[confluent] def getNonTxn[A](id: Id)(implicit format: ConstFormat[A]): A

  private[confluent] def removeFromCache(id: Id): Unit

  private[confluent] def addDirtyCache     (cache: Cache[T]): Unit
  private[confluent] def addDirtyLocalCache(cache: Cache[T]): Unit

  // private[confluent] def removeDurableIdMap[A](map: stm.IdentifierMap[Id, T, A]): Unit
}