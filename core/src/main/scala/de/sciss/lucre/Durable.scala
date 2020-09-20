package de.sciss.lucre

import de.sciss.lucre.impl.DurableImpl
import de.sciss.lucre.{DataStore, DurableLike, InMemory}

object Durable {
  def apply(factory: DataStore.Factory, mainName: String = "data"): Durable =
    DurableImpl(factory, mainName = mainName)

  def apply(mainStore: DataStore): Durable = 
    DurableImpl(mainStore = mainStore)

  trait Txn extends DurableLike.Txn[Txn]
}

trait Durable extends DurableLike[Durable.Txn] {
  final type I = InMemory
}