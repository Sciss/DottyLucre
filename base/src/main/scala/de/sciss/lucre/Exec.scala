/*
 *  Exec.scala
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

import de.sciss.serial.{DataInput, TFormat}

trait Exec[T <: Exec[T]] {
  type Id <: Ident[T]

  type I <: Exec[I]

  def inMemory: I

  implicit def inMemoryBridge: T => I

  def system: Base

  def newId(): Id

  def newRef[A](init: A): Ref[T, A]

  /** Creates a new in-memory transactional map for storing and retrieving values based on a mutable's identifier
   * as key. If a system is confluently persistent, the `get` operation will find the most recent key that
   * matches the search key. Objects are not serialized but kept live in memory.
   *
   * Id maps can be used by observing views to look up associated view meta data even though they may be
   * presented with a more recent access path of the model peer (e.g. when a recent event is fired and observed).
   *
   * @tparam A         the value type in the map
   */
  def newIdentMap[A]: IdentMap[T, A]

  def newInMemorySet[A]    : RefSet[T, A]
  def newInMemoryMap[A, B] : RefMap[T, A, B]

  def readId(in: DataInput): Id

  /** Creates a handle (in-memory) to refresh a stale version of an object, assuming that the future transaction is issued
   * from the same cursor that is used to create the handle, except for potentially having advanced.
   * This is a mechanism that can be used in live views to gain valid access to a referenced object
   * (e.g. self access).
   *
   * @param value         the object which will be refreshed when calling `get` on the returned handle
   * @param format    used to write and freshly read the object
   * @return              the handle
   */
  def newHandle[A](value: A)(implicit format: TFormat[T, A]): Source[T, A]
}

trait AnyExec extends Exec[AnyExec]