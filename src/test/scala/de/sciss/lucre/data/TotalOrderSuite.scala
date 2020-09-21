package de.sciss.lucre.data

import de.sciss.lucre.data.TotalOrder.Map.RelabelObserver
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Cursor, Durable, InMemory, Sys, TSerializer, TestUtil, Txn}
import de.sciss.serial.{DataInput, DataOutput, Writable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.immutable.{Vector => Vec}

/*

  To run this test copy + paste the following into sbt:

  testOnly de.sciss.lucre.data.TotalOrderSuite

 */
object TotalOrderSuite {
  object MapHolder {
    final class Serializer[T <: Txn[T]](observer: RelabelObserver[T, MapHolder[T]], tx0: T)
      extends TSerializer[T, MapHolder[T]] {

      val map: TotalOrder.Map[T, MapHolder[T]] = TotalOrder.Map.empty[T, MapHolder[T]](observer, _.entry)(tx0, this)

      def write(v: MapHolder[T], out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, tx: T)(implicit access: tx.Acc): MapHolder[T] = {
        val num = in.readInt()
        val e = map.readEntry(in, tx)
        new MapHolder(num, e)
      }
    }
  }
  final case class MapHolder[T <: Txn[T]](num: Int, entry: TotalOrder.Map.Entry[T, MapHolder[T]])
    extends Writable {

    def write(out: DataOutput): Unit = {
      out.writeInt(num)
      entry.write(out)
    }
  }
}
class TotalOrderSuite extends AnyFeatureSpec with GivenWhenThen {
  import TotalOrderSuite.MapHolder

  val IN_MEMORY        = true
  val DATABASE         = true
  val TEST1            = true   // large consistency test
  val TEST2            = true   // small boundary cases test

  val NUM = 0x10000    // 0x80000  // 0x200000
  val RND_SEED = 0L

  if (IN_MEMORY) {
    withSys[InMemory, InMemory.Txn]("Mem", { () => 
      val s = InMemory()
      (s, s)
    }, _ => ())
  }

  if (DATABASE) {
    withSys[Durable, Durable.Txn]("BDB", { () =>
      val bdb = BerkeleyDB.tmp() // open( dir )
      val s = Durable(bdb)
      (s, s)
    }, bdb => {
      val sz = bdb.step(bdb.numUserRecords(_))
      assert(sz == 0, "Final DB user size should be 0, but is " + sz)
      bdb.close()
    })
  }

  def withSys[S <: Sys, T <: Txn[T]](sysName: String, sysCreator: () => (S, Cursor[T]),
                           sysCleanUp: S => Unit): Unit = {
    def scenarioWithTime(description: String)(body: => Unit): Unit = {
      Scenario(description) {
        val t1 = System.currentTimeMillis()
        body
        val t2 = System.currentTimeMillis()
        println("For " + sysName + " the tests took " + TestUtil.formatSeconds((t2 - t1) * 0.001))
      }
    }

    if (TEST1) Feature("The ordering of the structure should be consistent") {
      info("Each two successive elements of the structure")
      info("should yield '<' in comparison")

      scenarioWithTime("Ordering is verified on a randomly filled " + sysName + " structure") {
        Given("a randomly filled structure (" + sysName + ")")

        val (system, cursor) = sysCreator()
        try {
          val to = cursor.step { implicit tx =>
            TotalOrder.Set.empty[T]()
          }
          val rnd = new util.Random(RND_SEED)
          // would be nice to test maximum possible number of labels
          // but we're running out of heap space ...
          val n = NUM // 113042 // 3041

          val set = cursor.step { implicit tx =>
            var e = to.root
            var coll = Set.empty[TotalOrder.Set.Entry[T]]
            for (_ <- 1 until n) {
              //if( (i % 1000) == 0 ) println( "i = " + i )
              if (rnd.nextBoolean()) {
                e = e.append() // to.insertAfter( e ) // to.insertAfter( i )
              } else {
                e = e.prepend() // to.insertBefore( e ) // e.prepend() // to.insertBefore( i )
              }
              coll += e
            }
            coll
          }

          When("the structure size is determined")
          val sz = cursor.step { implicit tx => to.size }
          Then("it should be equal to the number of elements inserted")
          assert(sz == n, sz.toString + " != " + n)

          When("the structure is mapped to its pairwise comparisons")
          val result = cursor.step { implicit tx =>
            var res = Set.empty[Int]
            var prev = to.head
            var next = prev.next.orNull
            while (next != null) {
              //                  res     += prev compare next
              res += prev.tag compare next.tag
              prev = next
              next = next.next.orNull
            }
            res
          }

          Then("the resulting set should only contain -1")
          assert(result == Set(-1), result.toString + " -- " + cursor.step(implicit tx => to.tagList(to.head)))

          When("the structure is emptied")
          val sz2 = cursor.step { implicit tx =>
            //                  set.foreach( _.remove() )
            set.foreach(_.removeAndDispose())
            to.size
          }
          Then("the order should have size 1")
          assert(sz2 == 1, "Size is " + sz2 + " and not 1")

          cursor.step { implicit tx =>
            to.dispose()
          }

        } finally {
          sysCleanUp(system)
        }
      }
    }

    if (TEST2) Feature("The structure should accept boundary cases") {
      scenarioWithTime(s"Triggering overflows at the boundaries in a $sysName structure") {
        val (system, cursor) = sysCreator()
        try {
          Given("an empty map structure")

          val order = cursor.step { implicit tx =>
            val ser = new MapHolder.Serializer(new RelabelObserver[T, MapHolder[T]] {
              def beforeRelabeling(dirty: Iterator[MapHolder[T]])(implicit tx: T): Unit = {
                dirty.toIndexedSeq // just to make sure the iterator succeeds
              }

              def afterRelabeling(clean: Iterator[MapHolder[T]])(implicit tx: T): Unit = {
                clean.toIndexedSeq // just to make sure the iterator succeeds
              }
            }, tx)
            ser.map
          }

          When("the structure is filled by 1000x repeated appending")
          val rootHolder = new MapHolder[T](0, order.root)
          var e = rootHolder
          var holders = Vec(e)
          for (i <- 1 to 1000) {
            e = cursor.step { implicit tx =>
              val prev = e
              val next = new MapHolder(i, order.insert())
              order.placeAfter(prev, next)
              next
            }
            holders :+= e
          }
          Then("the resulting sequence should match (0 to 1000)")
          val checkApp = cursor.step { implicit tx =>
            holders.sliding(2, 1).forall {
              case Seq(prev, next) => prev.num.compare(next.num) == prev.entry.compare(next.entry)
            }
          }
          assert(checkApp)

          When("all elements are removed")
          val szApp = cursor.step { implicit tx =>
            holders.drop(1).foreach { h =>
              h.entry.removeAndDispose()
            }
            order.size
          }
          Then("the structure should have size 1 (root)")
          assert(szApp === 1)

          When("the structure is filled by 1000x repeated prepending")
          e = rootHolder
          holders = Vec(e)
          for (i <- 1 to 1000) {
            e = cursor.step { implicit tx =>
              val prev = e
              val next = new MapHolder(i, order.insert())
              order.placeBefore(prev, next)
              next
            }
            holders :+= e
          }
          Then("the resulting sequence should match (0 to 1000)")
          val checkPrep = cursor.step { implicit tx =>
            holders.sliding(2, 1).forall {
              case Seq(prev, next) => next.num.compare(prev.num) == prev.entry.compare(next.entry)
            }
          }
          assert(checkPrep)

          When("all elements are removed")
          val szPrep = cursor.step { implicit tx =>
            holders.drop(1).foreach { h =>
              h.entry.removeAndDispose()
            }
            order.size
          }
          Then("the structure should have size 1 (root)")
          assert(szPrep === 1)

          // dispose
          cursor.step { implicit tx => order.dispose() }

        } finally {
          sysCleanUp(system)
        }
      }
    }
  }
}