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

package de.sciss.lucre

import java.io.Closeable

/** The `Base` trait is a pre-stage to `stm.Sys`, without introducing
 * peer STM transactions. It can thus be used to build purely imperative
 * non-transactional systems.
 *
 * @tparam S   the representation type of the system
 */
trait Base /*[Tx <: Exec[Tx]]*/ extends Closeable {
  //  type I <: Base[I]

  //  /** The transaction type of the system. */
  //  type Tx <: Executor

  //  def inMemory: I
  //  def inMemoryTx(tx: Tx): I#Tx

  /** Closes the underlying database (if the system is durable). The STM cannot be used beyond this call.
   * An in-memory system should have a no-op implementation.
   */
  def close(): Unit
}