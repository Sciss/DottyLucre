/*
 *  NNPerf.scala
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

import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D, IntPoint2DLike, IntSpace, IntSquare}
import de.sciss.lucre.InMemory

object NNPerf extends App {
  type S  = InMemory
  type T  = InMemory.Txn
  type D  = IntSpace.TwoDim
  implicit val space: D = IntSpace.TwoDim
  val i   = InMemory()
  var j   = 32
  val r   = new util.Random(0L)
  val down = 2
  val q     = IntPoint2D(0, 0)
  while (j <= 262144 * 4) {
    i.step { implicit tx =>
      val cube  = IntSquare(j >> 1, j >> 1, j >> 1)
      import SkipOctree.nonTxnPointView
      val tree  = SkipOctree.empty[T, IntPoint2DLike, IntSquare, IntPoint2D](cube)
      val ins   = (0 until j by down).map { i => IntPoint2D(i, j >> 1) }
      val v     = IntPoint2D(0, j - 1)
      //      val ins   = (0 until j by down).map { i => IntPoint2D(j >> 1, i) }
      //      val v     = IntPoint2D(j - 1, 0)

      //val ins   = Vector.fill(j)(IntPoint2D(r.nextInt(j), r.nextInt(j)))
      ins.foreach(tree += _)
      println(s"\nSize = $j")
      val res = tree.nearestNeighbor(v, IntDistanceMeasure2D.invertedChebyshev)
      val manual = ins.minBy(p => p.distanceSq(v))
      assert(res == manual, s"NN yields $res but manual is $manual")
    }

    j <<= 1
  }
}