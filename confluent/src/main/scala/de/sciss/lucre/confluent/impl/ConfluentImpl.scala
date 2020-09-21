/*
 *  ConfluentImpl.scala
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
package impl

import de.sciss.lucre.{Confluent, DataStore, Durable}

import scala.concurrent.stm.InTxn

object ConfluentImpl {
  def apply(storeFactory: DataStore.Factory): Confluent = {
    val mainStore   = storeFactory.open("data")
    val durable     = Durable(mainStore)
    new System(storeFactory, durable)
  }

  private final class System(protected val storeFactory: DataStore.Factory,
                             val durable: Durable)
    extends Mixin[Confluent.Txn] with Confluent {

//    def inMemory: I = durable.inMemory

    def durableTx (tx: T): D = ??? // tx.durable
    def inMemoryTx(tx: T): I = ??? // tx.inMemory

    protected def wrapRegular(dtx: D, inputAccess: Access[T], retroactive: Boolean, cursorCache: Cache[T],
                              systemTimeNanos: Long): T =
      new RegularTxn(this, dtx, inputAccess, retroactive, cursorCache)

    protected def wrapRoot(peer: InTxn): T = new RootTxn(this, peer)
  }
}