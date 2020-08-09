/*
 *  Executor.scala
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

package de.sciss.lucre.stm

import de.sciss.serial.{DataInput, Serializer}

trait Executor[S <: Base[S]] {
  /** Back link to the underlying system. */
  val system: S

  def newId(): S#Id

  // ---- variables ----

  def newRef[A](init: A): Ref[S#Tx, A]

  def newVar[A](id: S#Id, init: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): S#Var[A]
  def newBooleanVar(id: S#Id, init: Boolean): S#Var[Boolean]
  def newIntVar    (id: S#Id, init: Int    ): S#Var[Int]
  def newLongVar   (id: S#Id, init: Long   ): S#Var[Long]

  def newVarArray[A](size: Int): Array[S#Var[A]]

  /** Creates a new in-memory transactional map for storing and retrieving values based on a mutable's identifier
   * as key. If a system is confluently persistent, the `get` operation will find the most recent key that
   * matches the search key. Objects are not serialized but kept live in memory.
   *
   * Id maps can be used by observing views to look up associated view meta data even though they may be
   * presented with a more recent access path of the model peer (e.g. when a recent event is fired and observed).
   *
   * @tparam A         the value type in the map
   */
  def newInMemoryIdMap[A]: IdentifierMap[S#Id, S#Tx, A]

  def newInMemorySet[A]    : RefSet[S, A]
  def newInMemoryMap[A, B] : RefMap[S, A, B]

  def readVar[A](id: S#Id, in: DataInput)(implicit serializer: Serializer[S#Tx, S#Acc, A]): S#Var[A]
  def readBooleanVar(id: S#Id, in: DataInput): S#Var[Boolean]
  def readIntVar    (id: S#Id, in: DataInput): S#Var[Int]
  def readLongVar   (id: S#Id, in: DataInput): S#Var[Long]

  def readId(in: DataInput, acc: S#Acc): S#Id

  /** Creates a handle (in-memory) to refresh a stale version of an object, assuming that the future transaction is issued
   * from the same cursor that is used to create the handle, except for potentially having advanced.
   * This is a mechanism that can be used in live views to gain valid access to a referenced object
   * (e.g. self access).
   *
   * @param value         the object which will be refreshed when calling `get` on the returned handle
   * @param serializer    used to write and freshly read the object
   * @return              the handle
   */
  def newHandle[A](value: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A]
}