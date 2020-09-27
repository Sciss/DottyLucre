/*
 *  Space.scala
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

import de.sciss.serial.ConstFormat

object Space {
  final val bigZero = BigInt(0)
  final val bigOne  = BigInt(1)
}

/** A `Space` abstracts over the number of dimensions
  * that are used for point and hypercube operations.
  */
trait Space[P, H] {
  type Point <: P

  /** Given that the space is limited, this represents the farthest
    * point in the space, typically which each coordinate component
    * equal to `Int.MaxValue`.
    */
  def maxPoint: Point

  /** The number of dimensions in the space. */
  def dim: Int

  implicit def lexicalOrder: Ordering[P]

  implicit def pointFormat    : ConstFormat[Point]
  implicit def hyperCubeFormat: ConstFormat[H]
}