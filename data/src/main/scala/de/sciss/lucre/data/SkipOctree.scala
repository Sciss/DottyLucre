/*
 *  SkipOctree.scala
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
package data

import de.sciss.lucre.geom.{DistanceMeasure, HyperCube, QueryShape, Space}
import de.sciss.serial.{DataInput, TFormat, WritableFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

object SkipOctree {
  implicit def nonTxnPointView[P, A](implicit view: A => P): (A, Any) => P =
    (a, _) => view(a)

  def empty[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](hyperCube: H)
                                      (implicit tx: T, pointView: (A, T) => PL, space: Space[PL, P, H],
                                       keyFormat: TFormat[T, A]): SkipOctree[T, PL, P, H, A] =
    DetSkipOctree.empty[T, PL, P, H, A](hyperCube)

  def read[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](in: DataInput)
                                                         (implicit tx: T, pointView: (A, T) => PL,
                                                          space: Space[PL, P, H],
                                                          keyFormat: TFormat[T, A]): SkipOctree[T, PL, P, H, A] =
    DetSkipOctree.read[T, PL, P, H, A](in)

  implicit def format[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](implicit view: (A) => PL, space: Space[PL, P, H],
                              keyFormat: TFormat[T, A]): TFormat[T, SkipOctree[T, PL, P, H, A]] =
    new Fmt[T, PL, P, H, A]

  private final class Fmt[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](implicit view: (A) => PL, space: Space[PL, P, H],
                                                                         keyFormat: TFormat[T, A])
    extends WritableFormat[T, SkipOctree[T, PL, P, H, A]] {

    override def readT(in: DataInput)(implicit tx: T): SkipOctree[T, PL, P, H, A] =
      DetSkipOctree.read(in)

    override def toString = "SkipOctree.format"
  }
}

/** A `SkipOctree` is a multi-dimensional data structure that
 * maps coordinates to values. It extends the interface
 * of Scala's mutable `Map` and adds further operations such
 * as range requires and nearest neighbour search.
 */
trait SkipOctree[T <: Exec[T], PL, P, H, A] extends Mutable[T] {
  /** The space (i.e., resolution and dimensionality) underlying the tree. */
  def space: Space[PL, P, H]

  /** A function which maps an element (possibly through transactional access) to a geometric point coordinate. */
  def pointView: (A, T) => PL

  /** The base square of the tree. No point can lie outside this square (or hyper-cube). */
  def hyperCube: H

  /** Reports the number of decimation levels in the tree. */
  def numLevels(implicit tx: T): Int

  /** The number of orthants in each hyperCube. This is equal
   * to `1 << numDimensions` and gives the upper bound
   * of the index to `QNode.child()`.
   */
  def numOrthants: Int

  /** Queries the element at a given point.
   *
   * @param point  the point to look up.
   * @return `Some` element if found, `None` if the point was not present.
   */
  def get(point: PL)(implicit tx: T): Option[A]

  /** Queries whether an element is stored at a given point.
   *
   * @param point  the point to query
   * @return   `true` if an element is associated with the query point, `false` otherwise
   */
  def isDefinedAt(point: PL)(implicit tx: T): Boolean

  /** Removes the element stored under a given point view.
   *
   * @param   point the location of the element to remove
   * @return  the element removed, wrapped as `Some`, or `None` if no element was
   *          found for the given point.
   */
  def removeAt(point: PL)(implicit tx: T): Option[A]

  /** Queries the number of leaves in the tree. This may be a very costly action,
   * so it is recommended to only use it for debugging purposes.
   */
  def size(implicit tx: T): Int

  /** Adds an element to the tree (or replaces a given element with the same point location).
   *
   * @param   elem  the element to add
   * @return  `true` if the element is new in the tree. If a previous entry with the
   *          same point view is overwritten, this is `true` if the elements were
   *          '''not equal''', `false` if they were equal
   */
  def add(elem: A)(implicit tx: T): Boolean

  /** Looks up a point and applies a transformation to the entry associated with it.
   * This can be used to update an element in-place, or used for maintaining a spatial multi-map.
   *
   * @param point   the location at which to perform the transformation
   * @param fun  a function to transform the element found, or generate a new element. The argument is the
   *             element previously stored with the point, or `None` if no element is found. The result is
   *             expected to be `Some` new element to be stored, or `None` if no element is to be stored
   *             (in this case, if an element was previously stored, it is removed)
   * @return     the previously stored element (if any)
   */
  def transformAt(point: PL)(fun: Option[A] => Option[A])(implicit tx: T): Option[A]

  /** Removes an element from the tree
   *
   * @param   elem  the element to remove
   * @return  `true` if the element had been found in the tree and thus been removed.
   */
  def remove(elem: A)(implicit tx: T): Boolean

  /** Adds an element to the tree (or replaces a given element with the same point location).
   *
   * @param   elem  the element to add to the tree
   * @return  the old element stored for the same point view, if it existed
   */
  def update(elem: A)(implicit tx: T): Option[A]

  def rangeQuery[Area](qs: QueryShape[Area, PL, H])(implicit tx: T): Iterator[A]

  /** Tests whether the tree contains an element. */
  def contains(elem: A)(implicit tx: T): Boolean

  /** Tests whether the tree is empty (`true`) or whether it contains any elements (`false`). */
  def isEmpty(implicit tx: T): Boolean

  /** Converts the tree into a linearized indexed sequence. This is not necessarily a
   * very efficient method, and should usually just be used for debugging.
   */
  def toIndexedSeq(implicit tx: T): Vec[A]

  /** Converts the tree into a linearized list. This is not necessarily a
   * very efficient method, and should usually just be used for debugging.
   */
  def toList(implicit tx: T): List[A]

  /** Converts the tree into a linearized sequence. This is not necessarily a
   * very efficient method, and should usually just be used for debugging.
   * To avoid surprises, this does not call `iterator.toSeq` because that would
   * produce a `Stream` and thus subject to further changes to the tree while
   * traversing. The returned seq instead is 'forced' and thus stable.
   */
  def toSeq(implicit tx: T): Seq[A]

  /** Converts the tree into a non-transactional set. This is not necessarily a
   * very efficient method, and should usually just be used for debugging.
   */
  def toSet(implicit tx: T): Set[A]

  def clear()(implicit tx: T): Unit

  /** Reports the nearest neighbor entry with respect to
   * a given point.
   *
   * Note: There is a potential numeric overflow if the
   * squared distance of the query point towards the
   * furthest corner of the tree's root hyper-cube exceeds 63 bits.
   * For a root `IntSquare(0x40000000, 0x40000000, 0x40000000)`, this
   * happens for example for any point going more towards north-west
   * than `IntPoint2DLike(-1572067139, -1572067139)`.
   *
   * @param   point the point of which the nearest neighbor is to be found
   * @param   metric   (description missing)
   *
   * @throws  NoSuchElementException  if the tree is empty
   */
  def nearestNeighbor[M](point: PL, metric: DistanceMeasure[M, PL, H])(implicit tx: T): A

  /** Same as `nearestNeighbor` but returning an `Option`, thus not throwing an exception
   * if no neighbor is found.
   */
  def nearestNeighborOption[M](point: PL, metric: DistanceMeasure[M, PL, H])(implicit tx: T): Option[A]

  /** An `Iterator` which iterates over the points stored
   * in the octree, using an in-order traversal directed
   * by the orthant indices of the nodes of the tree.
   *
   * Great care has to be taken as the iterator might be corrupted if the tree
   * is successively changed before the iterator is exhausted.
   */
  def iterator(implicit tx: T): Iterator[A]

  /** Adds an element to the tree. If there is already an element stored at the point represented by the
   * new element, it will be replaced. */
  def +=(elem: A)(implicit tx: T): this.type

  /** Removes an element from the tree. If the element is not found, this operation does nothing. */
  def -=(elem: A)(implicit tx: T): this.type

  /** Returns a string debug representation of the octree. */
  def debugPrint()(implicit tx: T): String
}