/*
 *  IntCube.scala
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

import de.sciss.lucre.geom.IntSpace.ThreeDim
import de.sciss.serial.ConstFormat

/**
 * A three dimensional cube.
 *
 * Wikipedia: "Usually, the octant with all three positive coordinates is
 * referred to as the first octant. There is no generally used naming
 * convention for the other seven octants."
 *
 * However the article suggests (given that we count from zero):
 * - 0 (binary 000) - top-front-left
 * - 1 (binary 001) - top-back-right
 * - 2 (binary 010) - top-back-left
 * - 3 (binary 011) - top-front-left
 * - 4 (binary 100) - bottom-front-left
 * - 5 (binary 101) - bottom-back-left
 * - 6 (binary 110) - bottom-back-right
 * - 7 (binary 111) - bottom-front-right
 *
 * Obviously there is no clear connection between the orientation
 * and the binary representation. We thus prefer to chose the
 * the octants numbering in a binary fashion, assigning bit 0
 * to the x-axis, bit 1 to the y-axis, and bit 2 to
 * the z-axis, where top-front-left is 000, hence:
 *
 * - 0 (binary 000) - left-top-front
 * - 1 (binary 001) - right-top-front
 * - 2 (binary 010) - left-bottom-front
 * - 3 (binary 011) - right-bottom-front
 * - 4 (binary 100) - left-top-back
 * - 5 (binary 101) - right-top-back
 * - 6 (binary 110) - left-bottom-back
 * - 7 (binary 111) - right-bottom-back
 */
trait IntCubeLike extends HyperCube[IntPoint3DLike, IntCube] with QueryShape[BigInt, IntPoint3DLike, IntCube] {
  import Space.bigZero
  import ThreeDim.{HyperCube => Cube3D, _}

  /**
   * X coordinate of the cube's center
   */
  def cx: Int

  /**
   * Y coordinate of the cube's center
   */
  def cy: Int

  /**
   * Z coordinate of the cube's center
   */
  def cz: Int

  /**
   * The extent is the half side length of the cube
   */
  def extent: Int

  final def top   : Int = cy - extent
  final def left  : Int = cx - extent
  final def front : Int = cz - extent
  final def bottom: Int = cy + (extent - 1)
  final def right : Int = cx + (extent - 1)
  final def back  : Int = cz + (extent - 1)

  final def orthant(idx: Int): Cube3D = {
    val e   = extent >> 1
    val dx  = if ((idx & 1) == 0) -e else e
    val dy  = if ((idx & 2) == 0) -e else e
    val dz  = if ((idx & 4) == 0) -e else e
    IntCube(cx + dx, cy + dy, cz + dz, e)
  }

  final def containsP(point: PointLike): Boolean = {
    val em1 = extent - 1
    val px  = point.x
    val py  = point.y
    val pz  = point.z

    (cx - extent <= px) && (cx + em1 >= px) &&
    (cy - extent <= py) && (cy + em1 >= py) &&
    (cz - extent <= pz) && (cz + em1 >= pz)
  }

  final def containsH(cube: Cube3D): Boolean = {
    val bcx   = cube.cx
    val bcy   = cube.cy
    val bcz   = cube.cz
    val be    = cube.extent
    val bem1  = be - 1
    val em1   = extent - 1

    (bcx - be >= cx - extent) && (bcx + bem1 <= cx + em1) &&
      (bcy - be >= cy - extent) && (bcy + bem1 <= cy + em1) &&
      (bcz - be >= cz - extent) && (bcz + bem1 <= cz + em1)
  }

  final def area: BigInt = {
    val s = extent.toLong << 1
    BigInt(s * s) * BigInt(s)
  }

  // -- QueryShape --

  final def overlapArea(b: Cube3D): BigInt = {
    val bcx   = b.cx
    val bcy   = b.cy
    val bcz   = b.cz
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1

    val xMin = math.max(cx - extent, bcx - be).toLong
    val xMax = math.min(cx + em1, bcx + bem1).toLong
    val dx = xMax - xMin + 1
    if (dx <= 0L) return bigZero

    val yMin = math.max(cy - extent, bcy - be).toLong
    val yMax = math.min(cy + em1, bcy + bem1).toLong
    val dy = yMax - yMin + 1
    if (dy <= 0L) return bigZero

    val zMin = math.max(cz - extent, bcz - be).toLong
    val zMax = math.min(cz + em1, bcz + bem1).toLong
    val dz = zMax - zMin + 1
    if (dz <= 0L) return bigZero

    BigInt(dx * dy) * BigInt(dz)
  }

  final def isAreaGreater(a: Cube3D, b: BigInt): Boolean = a.area > b

  final def isAreaNonEmpty(area: BigInt): Boolean = area > bigZero

  final def minDistance(point: PointLike): Double = {
    math.sqrt(minDistanceSq(point).toDouble) // or use this: http://www.merriampark.com/bigsqrt.htm ?
  }

  final def maxDistance(point: PointLike): Double = {
    math.sqrt(maxDistanceSq(point).toDouble)
  }

  /**
   * The squared (euclidean) distance of the closest of the cube's corners
   * or sides to the point, if the point is outside the cube,
   * or zero, if the point is contained
   */
  final def minDistanceSq(point: PointLike): BigInt = {
    val ax  = point.x
    val ay  = point.y
    val az  = point.z
    val em1 = extent - 1

    val dx = if (ax < cx) {
      val xMin = cx - extent
      if (ax < xMin) xMin - ax else 0
    } else {
      val xMax = cx + em1
      if (ax > xMax) ax - xMax else 0
    }

    val dy = if (ay < cy) {
      val yMin = cy - extent
      if (ay < yMin) yMin - ay else 0
    } else {
      val yMax = cy + em1
      if (ay > yMax) ay - yMax else 0
    }

    val dz = if (az < cz) {
      val zMin = cz - extent
      if (az < zMin) zMin - az else 0
    } else {
      val zMax = cz + em1
      if (az > zMax) az - zMax else 0
    }

    if (dx == 0 && dy == 0 && dz == 0) bigZero
    else {
      val dxl = dx.toLong
      val dyl = dy.toLong
      val dzl = dz.toLong
      BigInt(dxl * dxl + dyl * dyl) + BigInt(dzl * dzl)
    }
  }

  /**
   * Calculates the maximum squared euclidean
   * distance to a point in the euclidean metric.
   * This is the distance (squared) to the corner which is the furthest from
   * the `point`, no matter if it lies within the hyper-cube or not.
   */
  final def maxDistanceSq(point: PointLike): BigInt = {
    val ax  = point.x
    val ay  = point.y
    val az  = point.z
    val em1 = extent - 1
    val axl = ax.toLong
    val ayl = ay.toLong
    val azl = az.toLong

    val dx = if (ax < cx) {
      (cx + em1).toLong - axl
    } else {
      axl - (cx - extent).toLong
    }

    val dy = if (ay < cy) {
      (cy + em1).toLong - ayl
    } else {
      ayl - (cy - extent).toLong
    }

    val dz = if (az < cz) {
      (cz + em1).toLong - azl
    } else {
      azl - (cz - extent).toLong
    }

    BigInt(dx * dx + dy * dy) + BigInt(dz * dz)
  }

  final def indexOfP(a: PointLike): Int = {
    val ax  = a.x
    val ay  = a.y
    val az  = a.z
    val em1 = extent - 1

    val xPos = if (ax < cx) {
      if (ax >= cx - extent) 0 else return -1
    } else {
      if (ax <= cx + em1) 1 else return -1
    }

    val yPos = if (ay < cy) {
      if (ay >= cy - extent) 0 else return -1
    } else {
      if (ay <= cy + em1) 2 else return -1
    }

    val zPos = if (az < cz) {
      if (az >= cz - extent) 0 else return -1
    } else {
      if (az <= cz + em1) 4 else return -1
    }

    xPos | yPos | zPos
  }

  final def indexOfH(b: Cube3D): Int = {
    val bcx   = b.cx
    val bcy   = b.cy
    val bcz   = b.cz
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1

    val bxMin = bcx - be
    val bxMax = bcx + bem1
    val xPos  = if (bcx < cx) {
      // left?
      // not particular elegant to return in an assignment, but well, it's allowed :)
      if ((bxMin >= cx - extent) && (bxMax < cx)) 0 else return -1
    } else {
      // right?
      if ((bxMin >= cx) && (bxMax <= cx + em1)) 1 else return -1
    }

    val byMin = bcy - be
    val byMax = bcy + bem1
    val yPos  = if (bcy < cy) {
      // top?
      if ((byMin >= cy - extent) && (byMax < cy)) 0 else return -1
    } else {
      // bottom?
      if ((byMin >= cy) && (byMax <= cy + em1)) 2 else return -1
    }

    val bzMin = bcz - be
    val bzMax = bcz + bem1
    val zPos = if (bcz < cz) {
      // front?
      if ((bzMin >= cz - extent) && (bzMax < cz)) 0 else return -1
    } else {
      // back?
      if ((bzMin >= cz) && (bzMax <= cz + em1)) 4 else return -1
    }

    xPos | yPos | zPos
  }

  final def greatestInterestingP(a: PointLike, b: PointLike): Cube3D = gi(a.x, a.y, a.z, 1, b)

  final def greatestInterestingH(a: Cube3D, b: PointLike): Cube3D = {
    val ae = a.extent
    gi(a.cx - ae, a.cy - ae, a.cz - ae, ae << 1, b)
  }

  private[this] def gi(aLeft: Int, aTop: Int, aFront: Int, aSize: Int, b: PointLike): Cube3D = {
    val tlx = cx - extent
    val tly = cy - extent
    val tlz = cz - extent
    val akx = aLeft - tlx
    val aky = aTop - tly
    val akz = aFront - tlz
    val bkx = b.x - tlx
    val bky = b.y - tly
    val bkz = b.z - tlz

    var x0 = 0
    var x1 = 0
    var x2 = 0
    if (akx <= bkx) {
      x0 = akx
      x1 = akx + aSize
      x2 = bkx
    } else {
      x0 = bkx
      x1 = bkx + 1
      x2 = akx
    }
    val mx = IntSpace.binSplit(x1, x2)

    var y0 = 0
    var y1 = 0
    var y2 = 0
    if (aky <= bky) {
      y0 = aky
      y1 = aky + aSize
      y2 = bky
    } else {
      y0 = bky
      y1 = bky + 1
      y2 = aky
    }
    val my = IntSpace.binSplit(y1, y2)

    var z0 = 0
    var z1 = 0
    var z2 = 0
    if (akz <= bkz) {
      z0 = akz
      z1 = akz + aSize
      z2 = bkz
    } else {
      z0 = bkz
      z1 = bkz + 1
      z2 = akz
    }
    val mz = IntSpace.binSplit(z1, z2)

    // that means the x extent is greater (x grid more coarse).
    if (mx <= my) {
      if (mx <= mz) {
        val mxs = mx << 1
        IntCube(tlx + (x2 & mx), tly + (y0 & mxs) - mx, tlz + (z0 & mxs) - mx, -mx)
      } else {
        val mzs = mz << 1
        IntCube(tlx + (x0 & mzs) - mz, tly + (y0 & mzs) - mz, tlz + (z2 & mz), -mz)
      }
    } else {
      if (my <= mz) {
        val mys = my << 1
        IntCube(tlx + (x0 & mys) - my, tly + (y2 & my), tlz + (z0 & mys) - my, -my)
      } else {
        val mzs = mz << 1
        IntCube(tlx + (x0 & mzs) - mz, tly + (y0 & mzs) - mz, tlz + (z2 & mz), -mz)
      }
    }
  }
}

object IntCube {
  implicit def format: ConstFormat[IntCube] = IntSpace.ThreeDim.hyperCubeFormat
}
final case class IntCube(cx: Int, cy: Int, cz: Int, extent: Int)
  extends IntCubeLike