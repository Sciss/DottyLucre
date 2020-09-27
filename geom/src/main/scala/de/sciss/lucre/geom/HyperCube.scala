/*
 *  HyperCube.scala
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

trait HyperCube[-P, H] {

  def orthant(idx: Int): H

  def containsP(point: P): Boolean

  /**
   * Checks whether a given hyper-cube is fully contained in this hyper-cube.
   * This is also the case if their bounds full match.
   */
  def containsH(hyperCube: H): Boolean

  /**
   * Calculates the minimum distance to a point in the euclidean metric.
   * This calls `minDistanceSq` and then takes the square root.
   */
  def minDistance(point: P): Double

  /**
   * Calculates the maximum distance to a point in the euclidean metric.
   * This calls `maxDistanceSq` and then takes the square root.
   */
  def maxDistance(point: P): Double

  /**
   * Determines the orthant index of a point `point`.
   *
   * @return  the index of the orthant (beginning at 0), or -1 if `point` lies
   *          outside of this hyper-cube.
   */
  def indexOfP(point: P): Int

  /**
   * Determines the orthant index of another internal hyper-cube `inner`.
   *
   * @return  the index of the orthant (beginning at 0), or -1 if `inner` lies
   *          outside of this hyper-cube.
   */
  def indexOfH(inner: H): Int

  /**
   * Calculates the greatest interesting hyper-cube inside this hyper-cube which
   * contains both points `a` and `b`, and they occupy distinct orthants.
   */
  def greatestInterestingP(a: P, b: P): H

  /**
   * Calculates the greatest interesting hyper-cube inside this hyper-cube which
   * contains both hyper-cube `a` and point `b`, and they occupy distinct orthants.
   */
  def greatestInterestingH(a: H, b: P): H
}