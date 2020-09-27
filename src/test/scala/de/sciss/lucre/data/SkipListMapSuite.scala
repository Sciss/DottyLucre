/*
 *  SkipListMapSuite.scala
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

package de.sciss.lucre.data

import de.sciss.lucre.{InMemory, TestUtil}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.immutable.{Vector => Vec}

/*

  To run this test copy + paste the following into sbt:

  testOnly de.sciss.lucre.data.SkipListMapSuite

  */
class SkipListMapSuite extends AnyFeatureSpec with GivenWhenThen {
  val SEED  = 0L
  val N     = 1000

  type S    = InMemory
  type T    = InMemory.Txn
  val rnd   = new util.Random(SEED)

  def scenarioWithTime(name: String, description: String)(body: => Unit): Unit =
    Scenario(description) {
      val t1 = System.currentTimeMillis()
      body
      val t2 = System.currentTimeMillis()
      println("For " + name + " the tests took " + TestUtil.formatSeconds((t2 - t1) * 0.001))
    }

  Feature("The skip list map structure should be consistent") {
    info("Several mass operations on the structure")
    info("are tried and expected behaviour verified")

    val system = InMemory()
    def atomic[A](fun: T => A): A = system.step(fun)

    val map = atomic { implicit tx =>
      SkipList.Map.empty[T, Int, Int]
    }

    def onEmptyList(): Unit = {
      When("the (size, isEmpty, nonEmpty, height) are queried")
      val szTup = atomic {
        implicit tx => (map.size, map.isEmpty, map.nonEmpty, map.height)
      }
      Then("they should be (0, true, false, 0)")
      assert(szTup == ((0, true, false, 0)), "found " + szTup)

      When("the (floor, ceil, contains) of a number if queried")
      val flCl = atomic {
        implicit tx => (map.floor(42), map.ceil(42), map.contains(42))
      }
      Then("they should be (None, None, false)")
      assert(flCl == ((None, None, false)), "found " + flCl)

      When("the iterator and sequence methods are called")
      val collections = atomic {
        implicit tx => (map.iterator.isEmpty, map.toSeq.isEmpty, map.toSet.isEmpty, map.toIndexedSeq.isEmpty)
      }
      Then("they should all return empty collections")
      assert(collections == ((true, true, true, true)), "found " + collections)
    }

    scenarioWithTime("empty", "Consistency is verified on an empty map") {
      onEmptyList()
    }

    val seq = Vec.tabulate(N)(n => rnd.nextInt() -> n)

    scenarioWithTime("filled", "Consistency is verified on a randomly filled map") {
      atomic { implicit tx =>
        seq.foreach(map += _)
      }

      val maxHeight = math.ceil(math.log(N + 1) / math.log(map.minGap)).toInt

      When("the (size, isEmpty, nonEmpty, height) is queried")
      val (sz, isEmpty, nonEmpty, h) = atomic { implicit tx =>
        (map.size, map.isEmpty, map.nonEmpty, map.height)
      }
      Then("it should be (N, <=log_minGap(N+1)) (" + (N -> maxHeight) + ")")
      assert(sz == N, "found size " + sz)
      assert(!isEmpty && nonEmpty, "isEmpty = " + isEmpty + "; nonEmpty = " + nonEmpty)
      assert(h <= maxHeight, "found height " + h)

      When("the floor and ceil values for a random number of elements are queried")
      val q = Vec.fill(N)(rnd.nextInt())
      val fc = atomic { implicit tx =>
        q.map { v =>
          (map.floor(v).getOrElse((Int.MinValue, 0)),
            map.ceil (v).getOrElse((Int.MaxValue, 0)))
        }
      }
      Then("they should be the same as with brute force search")
      val brute = q.map { v =>
        seq.foldLeft(((Int.MinValue, 0), (Int.MaxValue, 0))) {
          case ((fl, cl), v2) =>
            (if (fl._1 >= v2._1 || v2._1 > v) fl else v2) -> (if (cl._1 <= v2._1 || v2._1 < v) cl else v2)
        }
      }
      assert(fc == brute, s"Differ: ${(fc.take(5), brute.take(5))}")

      When("the floor and ceil values for the elements are asked")
      val allHit = atomic { implicit tx =>
        seq.forall { tup =>
          map.floor (tup._1).contains(tup) &&
            map.ceil  (tup._1).contains(tup)
        }
      }
      Then("they should return the elements themselves")
      assert(allHit)

      When("get is called")
      val flat = atomic { implicit tx =>
        seq.flatMap { case (key, _ /*value*/) =>
          map.get(key)
        }
      }
      val seqValues = seq.map(_._2)
      Then("the correct values should be retrieved")
      assert(flat === seqValues)

      val seqKeys: Set[Int] = seq.iterator.map(_._1).toSet
      val keys = q.toSet -- seqKeys
      val notFound = atomic { implicit tx =>
        keys.flatMap(map.get)
      }
      assert(notFound === Set.empty)
    }

    scenarioWithTime("emptied", "Consistency is verified When emptying the map") {
      When("all elements are removed")
      val rem = atomic { implicit tx =>
        seq.flatMap(v => map.remove(v._1))
      }
      Then("the returned elements should be same as the seq")
      val seqValues = seq.map(_._2)
      assert(rem == seqValues, s"Differ: ${(rem.take(5), seqValues.take(5))}")

      onEmptyList()
    }
  }
}