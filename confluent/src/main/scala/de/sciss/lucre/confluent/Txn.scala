/*
 *  Txn.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre
import de.sciss.serial
import de.sciss.serial.{Serializer, ImmutableSerializer}

trait Txn[T <: Txn[T]] extends lucre.Txn[T] {
//  implicit def durable: S#D#Tx

  def inputAccess: Access[T]

  def info: VersionInfo.Modifiable

  def isRetroactive: Boolean

  /** The confluent handle is enhanced with the `meld` method. */
  def newHandleM[A](value: A)(implicit serializer: Serializer[T, Access[T], A]): Source[T, A]

  private[confluent] def readTreeVertexLevel(term: Long): Int
  private[confluent] def addInputVersion(path: Access[T]): Unit

  private[confluent] def putTxn[A](id: Id, value: A)(implicit ser: serial.Serializer[T, Access[T], A]): Unit
  private[confluent] def putNonTxn[A](id: Id, value: A)(implicit ser: ImmutableSerializer[A]): Unit

  private[confluent] def getTxn[A](id: Id)(implicit ser: serial.Serializer[T, Access[T], A]): A
  private[confluent] def getNonTxn[A](id: Id)(implicit ser: ImmutableSerializer[A]): A

//  private[confluent] def putPartial[A](id: Id, value: A)(implicit ser: serial.Serializer[T, Access[T], A]): Unit
//  private[confluent] def getPartial[A](id: Id)(implicit ser: serial.Serializer[T, Access[T], A]): A

  private[confluent] def removeFromCache(id: Id): Unit

  private[confluent] def addDirtyCache     (cache: Cache[T]): Unit
  private[confluent] def addDirtyLocalCache(cache: Cache[T]): Unit

  // private[confluent] def removeDurableIdMap[A](map: stm.IdentifierMap[Id, T, A]): Unit
}