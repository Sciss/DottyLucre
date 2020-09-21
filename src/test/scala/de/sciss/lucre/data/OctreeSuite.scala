package de.sciss.lucre.data

import de.sciss.lucre.geom.IntDistanceMeasure3D.MS
import de.sciss.lucre.geom.IntSpace.ThreeDim
import de.sciss.lucre.geom.{DistanceMeasure, IntCube, IntDistanceMeasure3D, IntPoint3D, IntPoint3DLike, QueryShape}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Cursor, Durable, InMemory, Sys, TestUtil, Txn}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.mutable.{Set => MSet}
import scala.util.control.NonFatal

/*

  To run this test copy + paste the following into sbt:

  testOnly de.sciss.lucre.data.OctreeSuite

 */
class OctreeSuite extends AnyFeatureSpec with GivenWhenThen {
  val CONSISTENCY     = true
  val RANGE_SEARCH    = true
  val NN_SEARCH       = true
  val REMOVAL         = true
  val IN_MEMORY       = true
  val DATABASE        = true

  val n       = 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
  val n2: Int = n >> 3    // 0x1000    // range query and nn

  val rnd = new util.Random(2L) // ( 12L )

  val cube: IntCube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)

  def withSys[S <: Sys, T <: Txn[T]](sysName: String, sysCreator: () => (S, Cursor[T]),
                                     sysCleanUp: (S, Boolean) => Unit): Unit = {
    withTree[T](sysName, () => {
      val (system, cursor) = sysCreator()
      val t = cursor.step { implicit tx =>
        import ThreeDim.pointSerializer
        implicit val pointView: (IntPoint3D, Any) => IntPoint3D = (p, _) => p
        DetSkipOctree.empty[T, IntPoint3DLike, IntPoint3D, IntCube, IntPoint3D](cube)
      }
      (cursor, t, succ => sysCleanUp(system, succ))
    })
  }

  if (IN_MEMORY) {
    withSys[InMemory, InMemory.Txn]("Mem", { () => 
      val s = InMemory()
      (s, s)
    }, (_, _) => ())
  }
  if (DATABASE) {
    //BerkeleyDB.DB_CONSOLE_LOG_LEVEL = "ALL"
    withSys[Durable, Durable.Txn]("BDB", () => {
      //      val dir = File.createTempFile("octree", "_database")
      //      dir.delete()
      //      dir.mkdir()
      //      println(dir.getAbsolutePath)
      val bdb = BerkeleyDB.tmp()
      val s = Durable(bdb)
      (s, s)
    }, {
      case (bdb, success) =>
        //         println( "FINAL DB SIZE = " + bdb.numUserRecords )
        if (success) {
          val sz = bdb.step(bdb.numUserRecords(_))
          //            if( sz != 0 ) bdb.step( implicit tx => bdb.debugListUserRecords() ).foreach( println )
          assert(sz == 0, s"Final DB user size should be 0, but is $sz")
        }
        bdb.close()
    })
  }

  val pointFun3D: Int => IntPoint3D = mask => IntPoint3D(rnd.nextInt() & mask, rnd.nextInt() & mask, rnd.nextInt() & mask)

  def randFill[T <: Txn[T], PL, P, H](t: SkipOctree[T, PL, P, H, P], m: MSet[P],
                                           pointFun: Int => P)(implicit cursor: Cursor[T]): Unit = {
    Given("a randomly filled structure")

    for (_ <- 0 until n) {
      val k = pointFun(0x7FFFFFFF)
      cursor.step { implicit tx =>
        t += k
      }
      m += k
    }
  }

  def verifyConsistency[T <: Txn[T], PL, P, H](t: DetSkipOctree[T, PL, P, H, P])
                                                   (implicit cursor: Cursor[T]): Unit = {
    When("the internals of the structure are checked")
    Then("they should be consistent with the underlying algorithm")

    val res = cursor.step { implicit tx => t.verifyConsistency(reportOnly = true) }
    assert(res.isEmpty, res.mkString("\n"))
  }

  def verifyElems[T <: Txn[T], PL, P, H](t: SkipOctree[T, PL, P, H, P],
                                              m: MSet[P])(implicit cursor: Cursor[T]): Unit = {
    When( "the structure t is compared to an independently maintained map m" )
    val onlyInM = cursor.step { implicit tx =>
      m.filterNot { e =>
        t.contains(e)
      }
    }
    val onlyInT = cursor.step { implicit tx => t.iterator.toList.filterNot(e => m.contains(e)) }
    val szT     = cursor.step { implicit tx => t.size }
    val szM     = m.size
    Then("all elements of m should be contained in t")
    assert(onlyInM.isEmpty, onlyInM.take(10).toString())
    Then("all elements of t should be contained in m")
    assert(onlyInT.isEmpty, onlyInT.take(10).toString())
    Then("both should report the same size")
    assert(szT == szM, "octree has size " + szT + " / map has size " + szM)
  }

  def verifyContainsNot[T <: Txn[T], PL, P, H](t: SkipOctree[T, PL, P, H, P],
                                                    m: MSet[P],
                                                    pointFun: Int => P)
                                                   (implicit cursor: Cursor[T]): Unit = {
    When("the structure t is queried for keys not in the independently maintained map m")
    var testSet = Set.empty[P]
    while (testSet.size < 100) {
      val x = pointFun(0xFFFFFFFF)
      if (!m.contains(x)) testSet += x
    }
    val inT = cursor.step { implicit tx =>
      testSet.filter { p =>
        t.contains(p)
      }
    }
    Then("none of them should be contained in t")
    assert(inT.isEmpty, inT.take(10).toString())
  }

  def verifyAddRemoveAll[T <: Txn[T], PL, P <: PL, H](t: SkipOctree[T, PL, P, H, P],
                                                     m: MSet[P])
                                                    (implicit cursor: Cursor[T]): Unit = {
    When("all elements of the independently maintained map are added again to t")
    val szBefore  = cursor.step { implicit tx => t.size }
    //println( "BEFORE " + t.cursor.step { implicit tx => t.toList })
    val newInT    = cursor.step { implicit tx => m.filter(e => t.update(e).isEmpty) }
    //println( "AFTER " + t.cursor.step { implicit tx => t.toList })
    val szAfter   = cursor.step { implicit tx => t.size }
    Then("all of the put operations should return 'Some'")
    assert(newInT.isEmpty, newInT.take(10).toString())
    Then("the size of t should not change")
    assert(szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter)

    When( "all elements of the independently maintained map are removed from t" )
    val keptInT   = cursor.step { implicit tx => m.filter(e => t.removeAt(e).isEmpty) }
    val szAfter2  = cursor.step { implicit tx => t.size }
    Then("all of the remove operations should return 'Some'")
    assert(keptInT.isEmpty, keptInT.take(10).toString())
    Then("the size of t should be zero")
    assert(szAfter2 == 0, szAfter2.toString)
  }

  val queryFun3D: (Int, Int, Int) => IntCube = (max, off, ext) =>
    IntCube(rnd.nextInt(max) - off, rnd.nextInt(max) - off, rnd.nextInt(max) - off, rnd.nextInt(ext))

  val sortFun3D: IntPoint3DLike => (Int, Int, Int) = p => (p.x, p.y, p.z)

  def verifyRangeSearch[T <: Txn[T], A, PL, P <: PL, H, Sort](t: SkipOctree[T, PL, P, H, P], m: MSet[P],
                                                             queryFun: (Int, Int, Int) => QueryShape[A, PL, H],
                                                             sortFun: PL => Sort)
                                                            (implicit ord: math.Ordering[Sort], cursor: Cursor[T]): Unit = {
    When("the octree is range searched")
    val qs = Seq.fill(n2)(queryFun(0x7FFFFFFF, 0x40000000, 0x40000000))
    val rangesT = cursor.step { implicit tx => qs.map(q => t.rangeQuery(q).toSet) }
    val ks      = m // keySet
    val rangesM = qs.map(q => ks.filter(q.containsP))
    Then( "the results should match brute force with the corresponding set" )
    rangesT.zip(rangesM).foreach { case (s1, s2) =>
      assert(s1 == s2, s1.toList.sortBy(sortFun).take(10).toString + " -- " +
        s2.toList.sortBy(sortFun).take(10))
    }
  }

  val pointFilter3D: IntPoint3DLike => Boolean = { p =>
    val dx = if( p.x < cube.cx ) (cube.cx + (cube.extent - 1)).toLong - p.x else p.x - (cube.cx - cube.extent)
    val dy = if( p.y < cube.cy ) (cube.cy + (cube.extent - 1)).toLong - p.y else p.y - (cube.cy - cube.extent)
    val dz = if( p.z < cube.cz ) (cube.cz + (cube.extent - 1)).toLong - p.z else p.z - (cube.cz - cube.extent)
    dx <= 0xB504F300L && dy <= 0xB504F300L && dz <= 0xB504F300L &&
      (dx * dx + dy * dy > 0L) &&
      (dx * dx + dz * dz > 0L) &&
      (dy * dy + dz * dz > 0L)
  }

  val euclideanDist3D: MS = IntDistanceMeasure3D.euclideanSq

  def verifyNN[T <: Txn[T], M, PL, P <: PL, H](t: SkipOctree[T, PL, P, H, P], m: MSet[P], pointFun: Int => P,
                                              pointFilter: PL => Boolean,
                                              euclideanDist: DistanceMeasure[M, PL, H])
                                             (implicit ord: math.Ordering[M], cursor: Cursor[T]): Unit = {

    When("the quadtree is searched for nearest neighbours")
    val ps0 = Seq.fill(n2)(pointFun(0xFFFFFFFF))
    // tricky: this guarantees that there are no 63 bit overflows,
    // while still allowing points outside the root hyperCube to enter the test
    val ps : Seq[P]          = ps0.filter(pointFilter)
    val nnT: Map[P, P] = cursor.step { implicit tx =>
      ps.iterator.map(p => p -> t.nearestNeighbor(p, euclideanDist)).toMap
    }
    val ks = m // .keySet
    //      val nnM: Map[ P, P ] = ps.map( p => p -> ks.minBy( _.distanceSq( p ))( t.space.bigOrdering ))( breakOut )
    val nnM: Map[P, P] = ps.iterator.map(p => p -> ks.minBy(p2 => euclideanDist.distance(p2, p))).toMap
    Then("the results should match brute force with the corresponding set")
    assert(nnT == nnM, {
      nnT.collect {
        case (q, v) if nnM(q) != v => (q, v, nnM(q))
      }.take(10).toString()
    })
  }

  def withTree[T <: Txn[T]](name: String,
                            tf: () => (Cursor[T],
                              DetSkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, IntPoint3D], Boolean => Unit)): Unit = {
    Feature("The " + name + " octree structure should be consistent") {
      info("Several mass operations on the structure")
      info("are tried and expected behaviour verified")

      def scenarioWithTime(descr: String)(body: => Unit): Unit =
        Scenario(descr) {
          val t1 = System.currentTimeMillis()
          body
          val t2 = System.currentTimeMillis()
          println("For " + name + " the tests took " + TestUtil.formatSeconds((t2 - t1) * 0.001))
        }

      scenarioWithTime("Consistency is verified on a randomly filled structure") {
        val (_cursor, t, cleanUp) = tf()
        implicit val cursor: Cursor[T] = _cursor
        var success = false
        try {
          val m = MSet.empty[ThreeDim#Point]

          randFill         [T, IntPoint3DLike, IntPoint3D, IntCube](t, m, pointFun3D)
          if (CONSISTENCY) verifyConsistency[T, IntPoint3DLike, IntPoint3D, IntCube](t)
          verifyElems      [T, IntPoint3DLike, IntPoint3D, IntCube](t, m)
          verifyContainsNot[T, IntPoint3DLike, IntPoint3D, IntCube](t, m, pointFun3D)

          if (RANGE_SEARCH) verifyRangeSearch [T, BigInt, IntPoint3DLike, IntPoint3D, IntCube, (Int, Int, Int)](t, m, queryFun3D, sortFun3D)
          if (NN_SEARCH   ) verifyNN          [T, BigInt, IntPoint3DLike, IntPoint3D, IntCube]                 (t, m, pointFun3D, pointFilter3D, euclideanDist3D)
          if (REMOVAL     ) verifyAddRemoveAll[T, IntPoint3DLike, IntPoint3D, IntCube]                         (t, m)

          cursor.step { implicit tx =>
            try {
              t.clear()
              t.dispose()
            } catch {
              case NonFatal(e) =>
                e.printStackTrace()
                throw e
            }
          }
          success = true

        } finally {
          cleanUp(success)
        }
      }
    }
  }
}