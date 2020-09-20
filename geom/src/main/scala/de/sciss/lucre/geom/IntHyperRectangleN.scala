/*
 *  IntRectangle.scala
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
package geom

import de.sciss.lucre.geom.IntSpace.NDim

import scala.collection.immutable.{IndexedSeq => Vec}

/** An n-dimensional rectangular query shape. */
trait IntHyperRectangleNLike extends QueryShape[BigInt, IntPointNLike, IntHyperCubeN] {
  type P = IntPointNLike
  type H = IntHyperCubeN
  
  def dim: Int

  def min(dim: Int): Int

  /** Maximum coordinate for given dimension. This is inclusive
    * (considered to be inside the shape).
    */
  def max(dim: Int): Int

  final def containsP(point: P): Boolean = {
    var i = 0
    while (i < dim) {
      val pc = point(i)
      if (min(i) > pc || max(i) < pc) return false
      i += 1
    }
    true
  }

  final def overlapArea(b: H): BigInt = {
    val be    = b.extent
    val bem1  = be - 1
    var prod  = Space.bigOne

    var i = 0
    while (i < dim) {
      val bcc   = b.center(i)
      val mn    = math.max(min(i), bcc - be).toLong
      val mx    = math.min(max(i), bcc + bem1).toLong
      val delta = mx - mn + 1
      if (delta <= 0L) return Space.bigZero
      prod *= delta
      i += 1
    }

    prod
  }

  final def isAreaGreater(a: H, b: BigInt): Boolean = a.area > b

  final def isAreaNonEmpty(area: BigInt): Boolean = area > Space.bigZero
}

/** An n-dimensional hyper rectangular.
  *
  * @param components pairs of minimum and maximum coordinates for each dimension. In each tuple,
  *                   `_1` must be `<=` `_2`. The maximum coordinates are inclusive (defining the maximum
  *                   coordinate **within** the shape), thus it is not possible to create rectangles with
  *                   side lengths of zero.
  */
final case class IntHyperRectangleN(components: Vec[(Int, Int)])
  extends IntHyperRectangleNLike {

  def dim: Int = components.size

  def min(dim: Int): Int = components(dim)._1
  def max(dim: Int): Int = components(dim)._1
}
