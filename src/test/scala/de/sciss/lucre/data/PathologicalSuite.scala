package de.sciss.lucre.data

import de.sciss.lucre.InMemory
import de.sciss.lucre.geom.{IntPoint2D, IntPoint2DLike, IntSpace, IntSquare}
import org.scalatest.funspec.AnyFunSpec

/*

  To run only this suite

  testOnly de.sciss.lucre.data.PathologicalSuite

  */
class PathologicalSuite extends AnyFunSpec {
  type S  = InMemory
  type T  = InMemory.Txn
  type D  = IntSpace.TwoDim
  implicit val view: (IntPoint2D, T) => IntPoint2D = (p, _) => p

  describe("Tree insertion") {
    it("should work") {
      val i: S = InMemory()
      implicit val space: D = IntSpace.TwoDim
      val j   = 8

      i.step { implicit tx =>
        val cube  = IntSquare(j >> 1, j >> 1, j >> 1)
        val tree  = SkipOctree.empty[T, IntPoint2DLike, IntSquare, IntPoint2D](cube)
        val ins   = (0 until j).map { i => IntPoint2D(j >> 1, i) }
        // val v     = IntPoint2D(j - 1, 0)

        //val ins   = Vector.fill(j)(IntPoint2D(r.nextInt(j), r.nextInt(j)))
        ins.foreach(tree += _)
        // tree.nearestNeighbor(v, IntDistanceMeasure2D.euclideanSq)
      }
    }
  }
}
