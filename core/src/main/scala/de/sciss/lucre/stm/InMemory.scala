/*
 *  InMemory.scala
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

object InMemory {
  trait Txn extends InMemoryLike.Txn[Txn]
  
  def apply(): InMemory = impl.InMemoryImpl()
}
/** A thin in-memory (non-durable) wrapper around Scala-STM. */
trait InMemory extends InMemoryLike[InMemory.Txn] {
//  final type Tx = InMemoryLike.Txn[InMemory]
  final type I  = InMemory
}