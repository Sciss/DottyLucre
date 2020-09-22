/*
 *  IntPoint3D.scala
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

trait IntPoint3DLike {
  import IntSpace.ThreeDim._

  def x: Int
  def y: Int
  def z: Int

  def distanceSq(that: PointLike): BigInt = {
    val dx = that.x.toLong - x.toLong
    val dy = that.y.toLong - y.toLong
    val dz = that.z.toLong - z.toLong
    BigInt(dx * dx + dy * dy) + BigInt(dz * dz)
  }
}

object IntPoint3D {
  implicit def serializer: ConstantSerializer[IntPoint3D] = IntSpace.ThreeDim.pointSerializer
}
final case class IntPoint3D(x: Int, y: Int, z: Int) extends IntPoint3DLike {
  def +(p: IntPoint3D) = IntPoint3D(x + p.x, y + p.y, z + p.z)
  def -(p: IntPoint3D) = IntPoint3D(x - p.x, y - p.y, z - p.z)
}
