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
import de.sciss.lucre.{Confluent, NewImmutSerializer, TSerializer, TSource, confluent}

trait Txn[T <: Txn[T]] extends lucre.Txn[T] {
  def system: Confluent

  //  implicit def durable: S#D#Tx
  type Id     = Ident[T]
  type Acc    = Access[T]
  type Var[A] = confluent.Var[A]

  def inputAccess: Acc

  def info: VersionInfo.Modifiable

  def isRetroactive: Boolean

  /** The confluent handle is enhanced with the `meld` method. */
  def newHandleM[A](value: A)(implicit serializer: TSerializer[T, A]): TSource[T, A]

  private[confluent] def readTreeVertexLevel(term: Long): Int
  private[confluent] def addInputVersion(path: Acc): Unit

  private[confluent] def putTxn[A](id: Id, value: A)(implicit ser: TSerializer[T, A]): Unit
  private[confluent] def putNonTxn[A](id: Id, value: A)(implicit ser: NewImmutSerializer[A]): Unit

  private[confluent] def getTxn[A](id: Id)(implicit ser: TSerializer[T, A]): A
  private[confluent] def getNonTxn[A](id: Id)(implicit ser: NewImmutSerializer[A]): A

//  private[confluent] def putPartial[A](id: Id, value: A)(implicit ser: serial.Serializer[T, Access[T], A]): Unit
//  private[confluent] def getPartial[A](id: Id)(implicit ser: serial.Serializer[T, Access[T], A]): A

  private[confluent] def removeFromCache(id: Id): Unit

  private[confluent] def addDirtyCache     (cache: Cache[T]): Unit
  private[confluent] def addDirtyLocalCache(cache: Cache[T]): Unit

  // private[confluent] def removeDurableIdMap[A](map: stm.IdentifierMap[Id, T, A]): Unit
}