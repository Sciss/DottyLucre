/*
 *  Base.scala
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

import java.io.Closeable

import de.sciss.lucre.stm

/** The `Base` trait is a pre-stage to `stm.Sys`, without introducing
 * peer STM transactions. It can thus be used to build purely imperative
 * non-transactional systems.
 *
 * @tparam S   the representation type of the system
 */
trait Base[S <: Base[S]] extends Closeable {
  type I <: Base[I]

  def inMemory: I
  def inMemoryTx(tx: S#Tx): I#Tx

  /** The variable type of the system. Variables allow transactional storage and
   * retrieval both of immutable and mutable values. Specific systems may extend
   * the minimum capabilities described by the `Var` trait.
   *
   * @tparam A   the type of the value stored in the variable
   */
  type Var[A] <: stm.Var[S#Tx, A]

  /** The transaction type of the system. */
  type Tx <: Executor[S]

  /** The identifier type of the system. This is an opaque type about which the
   * user only knows that it uniquely identifies and object (or an object along
   * with its access path in the confluent case). It is thus valid to assume
   * that two objects are equal if their identifiers are equal.
   */
  type Id <: Identifier[S#Tx]

  /** The path access type for objects if they carry a temporal trace. This is
   * used by confluently persistent systems, while it is typically `Unit` for
   * ephemeral systems.
   */
  type Acc

  /** Closes the underlying database (if the system is durable). The STM cannot be used beyond this call.
   * An in-memory system should have a no-op implementation.
   */
  def close(): Unit
}

trait NoBase extends Base[NoBase]