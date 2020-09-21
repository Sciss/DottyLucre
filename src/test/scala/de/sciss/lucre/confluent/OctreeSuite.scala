package de.sciss.lucre.confluent

import java.io.File

import de.sciss.lucre.data.{DetSkipOctree, SkipOctree}
import de.sciss.lucre.geom.IntDistanceMeasure3D.MS
import de.sciss.lucre.geom.IntSpace.ThreeDim
import de.sciss.lucre.geom.{DistanceMeasure, HyperCube, IntCube, IntDistanceMeasure3D, IntPoint3D, IntPoint3DLike, QueryShape}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, ConfluentLike, InTxnRandom, TRandom, TestUtil, Cursor => LCursor, TSource => LTSource}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.stm.InTxn

/*

  To run this test copy + paste the following into sbt:

  testOnly de.sciss.lucre.confluent.OctreeSuite

 */
class OctreeSuite extends AnyFeatureSpec with GivenWhenThen {
  val CONSISTENCY   = true
  val RANGE_SEARCH  = true
  val NN_SEARCH     = true
  val REMOVAL       = true

  val n             = 0x800   // too slow -- 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
  val n2: Int       = n >> 3  // 0x1000    // range query and nn

  val rnd: TRandom[InTxn] = InTxnRandom(2L) // ( 12L )

  val cube: IntCube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)

  // make sure we don't look tens of thousands of actions
  Log.showLog = false

  def withSys[S <: ConfluentLike[T], T <: Txn[T]](sysName: String, sysCreator: () => S, sysCleanUp: (S, Boolean) => Unit): Unit = {
    withTree[T](sysName, () => {
      val system = sysCreator()
      // import SpaceSerializers.{IntPoint3DSerializer, IntCubeSerializer}
//      implicit val pointView = (p: IntPoint3D /*, _: Any*/ ) => p
//      implicit val ser = DetSkipOctree.serializer[T, IntPoint3DLike, IntPoint3D, IntCube, IntPoint3D]
      val (access, cursor) = system.cursorRoot { implicit tx =>
        DetSkipOctree.empty[T, IntPoint3DLike, IntPoint3D, IntCube, IntPoint3D](cube)
      } {
        implicit tx => _ => system.newCursor()
      }

      (cursor, access, succ => sysCleanUp(system, succ))
    })
  }

  withSys[Confluent, Confluent.Txn]("Confluent", () => {
    val dir = File.createTempFile("totalorder", "_database")
    dir.delete()
    dir.mkdir()
    println(dir.getAbsolutePath)
    val store = BerkeleyDB.factory(dir)
    val res = Confluent(store)
    //      res.root[ Unit ] { _ => }
    res
  }, (s, _ /* success */) => {
    //      val sz = bdb.step( bdb.numUserRecords( _ ))
    ////         println( "FINAL DB SIZE = " + sz )
    //      assert( sz == 0, "Final DB user size should be 0, but is " + sz )
    //      bdb.close()
    s.close()
  })

  val pointFun3D: (InTxn) => (Int) => IntPoint3D = itx => mask => IntPoint3D(
    rnd.nextInt()(itx) & mask,
    rnd.nextInt()(itx) & mask,
    rnd.nextInt()(itx) & mask
  )

  def randFill[T <: Txn[T], PL, P <: PL, H](access: LTSource[T, SkipOctree[T, PL, P, H, P]],
                                           m: MSet[P],
                                           pointFun: InTxn => Int => P)(implicit cursor: LCursor[T]): Unit = {
    Given("a randomly filled structure")

    for (_ <- 0 until n) {
      //         if( i == n - 1 ) {
      //            println( "here" )
      //         }
      m += cursor.step { implicit tx =>
        val k = pointFun(tx.peer)(0x7FFFFFFF)
        access() += k
        k
      }
    }
  }

  def verifyConsistency[T <: Txn[T], PL, P <: PL, H <: HyperCube[PL, H]](access: LTSource[T, DetSkipOctree[T, PL, P, H, P]])
                                                                 (implicit cursor: LCursor[T]): Unit = {
    When("the internals of the structure are checked")
    Then("they should be consistent with the underlying algorithm")

    type Branch = DetSkipOctree.Branch[T, PL, P, H, P]
    type Leaf   = DetSkipOctree.Leaf  [T, PL, P, H, P]

    val (q, h0, numOrthants) = cursor.step { implicit tx =>
      val _t = access()
      (_t.hyperCube, _t.lastTree, _t.numOrthants)
    }
    var currUnlinkedOcs  = Set.empty[H ]
    var currPoints       = Set.empty[PL]
    var prevs = 0
    var h: Branch = h0
    do {
      assert(h.hyperCube == q, s"Root level quad is ${h.hyperCube} while it should be $q in level n - $prevs")
      val nextUnlinkedOcs  = currUnlinkedOcs
      val nextPoints       = currPoints
      currUnlinkedOcs      = Set.empty
      currPoints           = Set.empty

      def checkChildren(n: Branch, depth: Int)(implicit tx: T): Unit = {
        def assertInfo = " in level n-" + prevs + " / depth " + depth

        var i = 0; while (i < numOrthants) {
          n.child( i ) match {
            case cb: Branch =>
              val nq = n.hyperCube.orthant(i)
              val cq = cb.hyperCube
              assert( nq.containsH(cq), s"Node has invalid hyper-cube ($cq), expected: $nq$assertInfo")
              assert( n.hyperCube.indexOfH(cq) == i, "Mismatch between index-of and used orthant (" + i + "), with parent " + n.hyperCube + " and " + cq )
              cb.nextOption match {
                case Some( next ) =>
                  assert( next.prevOption.contains(cb), "Asymmetric next link " + cq + assertInfo )
                  assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                case None =>
                  assert( !nextUnlinkedOcs.contains( cq ), "Double missing link for " + cq + assertInfo )
              }
              cb.prevOption match {
                case Some( prev ) =>
                  assert( prev.nextOption.contains(cb), "Asymmetric prev link " + cq + assertInfo )
                  assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                case None => currUnlinkedOcs += cq
              }
              checkChildren( cb, depth + 1 )

            case l: Leaf =>
              currPoints += l.value

            case _ =>
          }
          i += 1 }
      }
      cursor.step { implicit tx => checkChildren( h, 0 )}
      val pointsOnlyInNext    = nextPoints.diff(currPoints)
      assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
      h = h.prevOption.orNull
      prevs += 1
    } while( h != null )
  }

  def verifyElems[T <: Txn[T], PL, P <: PL, H](access: LTSource[T, SkipOctree[T, PL, P, H, P]],
                                              m: MSet[P])(implicit cursor: LCursor[T]): Unit = {
    When( "the structure t is compared to an independently maintained map m" )
    val onlyInM  = cursor.step { implicit tx =>
      val t = access()
      m.filterNot { e => t.contains(e) }
    }
    val onlyInT  = cursor.step { implicit tx => access().iterator.toList.filterNot(e => m.contains(e)) }
    val szT      = cursor.step { implicit tx => access().size }
    val szM      = m.size
    Then("all elements of m should be contained in t")
    assert(onlyInM.isEmpty, onlyInM.take(10).toString())
    Then("all elements of t should be contained in m")
    assert(onlyInT.isEmpty, onlyInT.take(10).toString())
    Then("both should report the same size")
    assert(szT == szM, "octree has size " + szT + " / map has size " + szM)
  }

  def verifyContainsNot[T <: Txn[T], PL, P <: PL, H](access: LTSource[T, SkipOctree[T, PL, P, H, P]],
                                                    m: MSet[P],
                                                    pointFun: InTxn => Int => P)
                                                   (implicit cursor: LCursor[T]): Unit = {
    When( "the structure t is queried for keys not in the independently maintained map m" )
    var testSet = Set.empty[P]
    while (testSet.size < 100) {
      val x = cursor.step(tx => pointFun(tx.peer)(0xFFFFFFFF))
      if (!m.contains(x)) testSet += x
    }
    val inT = cursor.step { implicit tx =>
      val t = access()
      testSet.filter { p => t.contains(p) }
    }
    Then("none of them should be contained in t")
    assert(inT.isEmpty, inT.take(10).toString())
  }

  def verifyAddRemoveAll[T <: Txn[T], PL, P <: PL, H](access: LTSource[T, SkipOctree[T, PL, P, H, P]],
                                                     m: MSet[P])(implicit cursor: LCursor[T]): Unit = {

    When( "all elements of the independently maintained map are added again to t" )
    val szBefore = cursor.step { implicit tx => access().size }
    //println( "BEFORE " + t.system.step { implicit tx => t.toList })

    //LucreConfluent.showLog = true

    val newInT   = cursor.step { implicit tx =>
      val t = access()
      val res = m.filter({ e =>
        t.update( e ).isEmpty
      })
      //         println( "Aqui" )
      res
    }
    //println( "AFTER " + t.system.step { implicit tx => t.toList })
    val szAfter  = cursor.step { implicit tx => access().size }
    Then("all of the put operations should return 'Some'")
    assert(newInT.isEmpty, newInT.take(10).toString())
    Then("the size of t should not change")
    assert(szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter)

    When("all elements of the independently maintained map are removed from t")

    //LucreConfluent.showLog = true

    val keptInT  = cursor.step { implicit tx =>
      val t = access()
      m.filter(e => t.removeAt(e).isEmpty)
    }
    //      val keptInT = m.filter( e => cursor.step { implicit tx => access.get.removeAt( e ).isEmpty })

    val szAfter2 = cursor.step { implicit tx => access().size }
    Then("all of the remove operations should return 'Some'")
    assert(keptInT.isEmpty, keptInT.take(10).toString())
    Then("the size of t should be zero")
    assert(szAfter2 == 0, szAfter2.toString)
  }

  val queryFun3D: (InTxn) => (Int, Int, Int) => IntCube = (itx: InTxn) => (max, off, ext) =>
    IntCube(
      rnd.nextInt(max)(itx) - off,
      rnd.nextInt(max)(itx) - off,
      rnd.nextInt(max)(itx) - off,
      rnd.nextInt(ext)(itx)
    )

  val sortFun3D: (IntPoint3DLike) => (Int, Int, Int) = p => (p.x, p.y, p.z)

  def verifyRangeSearch[T <: Txn[T], A, PL, P <: PL, H, Sort](
                                                              access: LTSource[T, SkipOctree[T, PL, P, H, P]], m: MSet[P],
                                                              queryFun: InTxn => (Int, Int, Int) => QueryShape[A, PL, H],
                                                              sortFun: PL => Sort)(implicit ord: math.Ordering[Sort], cursor: LCursor[T]): Unit = {

    When( "the octree is range searched" )
    val qs = cursor.step { tx =>
      val f = queryFun(tx.peer)
      Seq.fill(n2)(f(0x7FFFFFFF, 0x40000000, 0x40000000))
    }
    val rangesT = cursor.step { implicit tx =>
      val t = access()
      qs.map(q => t.rangeQuery(q).toSet)
    }
    val ks      = m // keySet
    val rangesM = qs.map(q => ks.filter(q.containsP(_)))
    Then( "the results should match brute force with the corresponding set" )
    rangesT.zip(rangesM).foreach { case (s1, s2) =>
      assert( s1 == s2, s1.toList.sortBy( sortFun ).take( 10 ).toString + " -- " +
        s2.toList.sortBy(sortFun).take(10))
    }
  }

  val pointFilter3D: (IntPoint3DLike) => Boolean = { p =>
    val dx = if (p.x < cube.cx) (cube.cx + (cube.extent - 1)).toLong - p.x else p.x - (cube.cx - cube.extent)
    val dy = if (p.y < cube.cy) (cube.cy + (cube.extent - 1)).toLong - p.y else p.y - (cube.cy - cube.extent)
    val dz = if (p.z < cube.cz) (cube.cz + (cube.extent - 1)).toLong - p.z else p.z - (cube.cz - cube.extent)
    dx <= 0xB504F300L && dy <= 0xB504F300L && dz <= 0xB504F300L &&
      dx * dx + dy * dy > 0L &&
      dx * dx + dz * dz > 0L &&
      dy * dy + dz * dz > 0L
  }

  val euclideanDist3D: MS = IntDistanceMeasure3D.euclideanSq

  def verifyNN[T <: Txn[T], M, PL, P <: PL, H](access: LTSource[T, SkipOctree[T, PL, P, H, P]], m: MSet[P],
                                              pointFun: InTxn => Int => P, pointFilter: PL => Boolean,
                                              euclideanDist: DistanceMeasure[M, PL, H])
                                             (implicit ord: math.Ordering[M], cursor: LCursor[T]): Unit = {

    When("the quadtree is searched for nearest neighbours")
    val ps0 = cursor.step { tx =>
      val f = pointFun(tx.peer)
      Seq.fill(n2)(f(0xFFFFFFFF))
    }
    // tricky: this guarantees that there are no 63 bit overflows,
    // while still allowing points outside the root hyperCube to enter the test
    val ps: Seq[P] = ps0.filter(pointFilter)
    val nnT: Map[P, P] = cursor.step { implicit tx =>
      val t = access()
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

  def withTree[T <: Txn[T]](name: String, tf: () => (LCursor[T],
    LTSource[T, DetSkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, IntPoint3D]], Boolean => Unit)): Unit = {

    type PL = IntPoint3DLike
    type P  = IntPoint3D
    type H  = IntCube

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
        val (_sys, access, cleanUp)  = tf()
        implicit val system: LCursor[T] = _sys
        var success = false
        try {
          val m = MSet.empty[ThreeDim#Point]

          randFill[T, PL, P, H](access, m, pointFun3D)
          if( CONSISTENCY ) verifyConsistency [T, PL, P, H](access)
          verifyElems       [T, PL, P, H](access, m)
          verifyContainsNot [T, PL, P, H](access, m, pointFun3D)

          if( RANGE_SEARCH) verifyRangeSearch [T, BigInt, PL, P, H, (Int, Int, Int)](access, m, queryFun3D, sortFun3D)
          if( NN_SEARCH   ) verifyNN          [T, BigInt, PL, P, H]                 (access, m, pointFun3D, pointFilter3D, euclideanDist3D)
          if( REMOVAL     ) verifyAddRemoveAll[T        , PL, P, H]                 (access, m)

          system.step { implicit tx =>
            val t = access()
            //                  try {
            t.clear()
            //                     t.dispose()
            //                  } catch {
            //                     case e =>
            //                        e.printStackTrace()
            //                        throw e
            //                  }
          }
          success = true

        } finally {
          cleanUp(success)
        }
      }
    }
  }
}