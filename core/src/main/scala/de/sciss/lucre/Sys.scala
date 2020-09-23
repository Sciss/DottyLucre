/*
 *  Sys.scala
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

import de.sciss.serial.TFormat

/** A system in LucreSTM describes a particular mode of representing values in time and of
 * persisting values to disk. The `Sys` trait contains types for variables, identifiers,
 * access paths, and transactions which are unique to each system (such as ephemeral in-memory,
 * confluently persistent etc.).
 */
trait Sys /*[S <: Sys[S]]*/ extends Base /*[S]*/ {
  //  type I <: InMemoryLike[I]

  /** The transaction type of the system. */
  type T <: Txn[T]

  /** Reads the root object representing the stored data structure,
   * or provides a newly initialized one via the `init` argument,
   * if no root has been stored yet.
   */
  def root[A](init: T => A)(implicit format: TFormat[T, A]): Source[T, A]

  private[lucre] def rootJoin[A](init: T => A)
                                (implicit tx: TxnLike, format: TFormat[T, A]): Source[T, A]

  // ---- event ----

  private[lucre] def reactionMap: ReactionMap[T]
}

//trait NoSys extends Sys[NoSys]