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

package de.sciss.lucre.experiment

import de.sciss.lucre.experiment
import de.sciss.serial.{DataInput, Serializer}

trait Exec[T <: Exec[T]] {
  //  type S <: Base[T]

  type Id <: Ident[T]
  type Acc

  type Var[A] <: experiment.Var[A]

  //  /** Back link to the underlying system. */
  //  val s: S

  def newId(): Id

  // ---- variables ----

//  def newRef[A](init: A): Ref[T, A]

//  def newVar[A](id: Id, init: A)(implicit serializer: TxSerializer[T,/* Acc,*/ A]): Var[A]
//
//  def newBooleanVar(id: Id, init: Boolean): Var[Boolean]
//  def newIntVar    (id: Id, init: Int    ): Var[Int]
//  def newLongVar   (id: Id, init: Long   ): Var[Long]

//  def newVarArray[A](size: Int): Array[Var[A]]

  /** Creates a new in-memory transactional map for storing and retrieving values based on a mutable's identifier
   * as key. If a system is confluently persistent, the `get` operation will find the most recent key that
   * matches the search key. Objects are not serialized but kept live in memory.
   *
   * Id maps can be used by observing views to look up associated view meta data even though they may be
   * presented with a more recent access path of the model peer (e.g. when a recent event is fired and observed).
   *
   * @tparam A         the value type in the map
   */
  def newIdentMap[A]: IdentMap[Id, T, A]

  //  def newInMemorySet[A]    : RefSet[S, A]
  //  def newInMemoryMap[A, B] : RefMap[S, A, B]

//  def readVar[A](id: Id, in: DataInput)(implicit serializer: TxSerializer[T, /*Acc,*/ A]): Var[A]
//
//  def readBooleanVar(id: Id, in: DataInput): Var[Boolean]
//  def readIntVar    (id: Id, in: DataInput): Var[Int]
//  def readLongVar   (id: Id, in: DataInput): Var[Long]

  def readId(in: DataInput)(implicit acc: Acc): Id

  /** Creates a handle (in-memory) to refresh a stale version of an object, assuming that the future transaction is issued
   * from the same cursor that is used to create the handle, except for potentially having advanced.
   * This is a mechanism that can be used in live views to gain valid access to a referenced object
   * (e.g. self access).
   *
   * @param value         the object which will be refreshed when calling `get` on the returned handle
   * @param serializer    used to write and freshly read the object
   * @return              the handle
   */
  def newHandle[A](value: A)(implicit serializer: TxSerializer[T, /*Acc,*/ A]): Handle[T, A]
}

trait AnyExec extends Exec[AnyExec]