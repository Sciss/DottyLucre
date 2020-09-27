/*
 *  CopySpec.scala
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

import de.sciss.lucre.store.BerkeleyDB
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*
  to run only this test

  test-only de.sciss.lucre.CopySpec

 */
class CopySpec extends FixtureAnyFlatSpec with Matchers {
  type T = Durable.Txn
  type S = Durable
  
  case class FixtureParam(s1: S, s2: S)

  protected def withFixture(test: OneArgTest): Outcome = {
    val s1 = Durable(BerkeleyDB.tmp())
    try {
      val s2 = Durable(BerkeleyDB.tmp())
      try {
        test(FixtureParam(s1, s2))
      } finally {
        s2.close()
      }
    } finally {
      s1.close()
    }
  }

  "Obj instances" should "be copyable across systems" in { param =>
    import param._
    val num1H = s1.step { implicit tx =>
      val num1 = IntObj.newVar(IntObj.newConst[T](1234))
      tx.newHandle(num1)
    }

    val num2H = Txn.copy[T, T, Source[T, IntObj[T]]] { (tx1, tx2) =>
      val num1 = num1H()(tx1)
      val num2 = Obj.copy[T, T, IntObj](num1)(tx1, tx2)
      tx2.newHandle(num2)
    } (s1, s2)

    s1.step { implicit tx =>
      val num = num1H()
      assert(num.value === 1234)
    }

    s2.step { implicit tx =>
      val num = num2H()
      assert(num.value === 1234)
    }
  }
}