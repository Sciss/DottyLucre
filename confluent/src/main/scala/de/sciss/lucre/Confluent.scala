/*
 *  Confluent.scala
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

import de.sciss.lucre.confluent.impl.{ConfluentImpl => Impl}
import de.sciss.lucre.{confluent, stm}

object Confluent {
  def apply(storeFactory: DataStore.Factory): Confluent = Impl(storeFactory)
  
  type Txn = confluent.Txn[Confluent]
}

trait Confluent extends Sys /*[Confluent]*/ {
  final protected type S = Confluent
  final type D = Durable
  final type I = InMemory
  final type T = confluent.Txn[S]
}