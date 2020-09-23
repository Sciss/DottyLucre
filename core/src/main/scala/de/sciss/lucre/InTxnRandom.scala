/*
 *  InTxnRandom.scala
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

import de.sciss.lucre.impl.RandomImpl.{BaseImpl, calcSeedUniquifier, initialScramble}

import scala.concurrent.stm.{InTxn, Ref => STMRef}

object InTxnRandom {
  def apply():           Random[InTxn] = apply(calcSeedUniquifier() ^ System.nanoTime())
  def apply(seed: Long): Random[InTxn] = new Impl(STMRef(initialScramble(seed)))

  private final class Impl(seedRef: STMRef[Long]) extends BaseImpl[InTxn] {
    def rawSeed_=(value: Long)(implicit tx: InTxn): Unit = seedRef() = value
    def rawSeed               (implicit tx: InTxn): Long = seedRef()
  }
}