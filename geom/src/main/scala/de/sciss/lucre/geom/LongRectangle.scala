/*
 *  LongRectangle.scala
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
package geom

/** A 2D rectangular query shape. */
trait LongRectangleLike extends QueryShape[BigInt, LongPoint2DLike, LongSquare] {
  import LongSpace.TwoDim.{HyperCube => Square, _}
  import Space.bigZero

  def left  : Long
  def top   : Long
  def width : Long
  def height: Long

  final def bottom: Long = top  + (height - 1)
  final def right : Long = left + (width  - 1)

  def containsP(point: PointLike): Boolean = {
    val px = point.x
    val py = point.y
    left <= px && right >= px && top <= py && bottom >= py
  }

  final def overlapArea(q: Square): BigInt = {
    val l = math.max(q.left , left )
    val r = math.min(q.right, right)
    val w = r - l + 1
    if (w <= 0L) return bigZero
    val t = math.max(q.top   , top   )
    val b = math.min(q.bottom, bottom)
    val h = b - t + 1
    if (h <= 0L) return bigZero
    BigInt(w) * BigInt(h)
  }

  final def isAreaGreater(a: Square, b: BigInt): Boolean = a.area > b

  def isAreaNonEmpty(area: BigInt): Boolean = area > bigZero
}

final case class LongRectangle(left: Long, top: Long, width: Long, height: Long) extends LongRectangleLike
