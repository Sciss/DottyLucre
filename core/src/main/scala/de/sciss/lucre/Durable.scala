/*
 *  Durable.scala
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

package de.sciss.lucre

import de.sciss.lucre.impl.DurableImpl

object Durable {
  def apply(factory: DataStore.Factory, mainName: String = "data"): Durable =
    DurableImpl(factory, mainName = mainName)

  def apply(mainStore: DataStore): Durable = 
    DurableImpl(mainStore = mainStore)

  trait Txn extends DurableLike.Txn[Txn] {
    final type I = InMemory.Txn
  }
}

trait Durable extends DurableLike[Durable.Txn] {
  final type I = InMemory.Txn

  override def inMemory: InMemory
}