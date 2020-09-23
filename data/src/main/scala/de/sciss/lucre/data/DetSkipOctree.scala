/*
 *  DetSkipOctree.scala
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

package de.sciss.lucre.data

import java.io.PrintStream

import de.sciss.lucre.geom.{DistanceMeasure, HyperCube, QueryShape, Space}
import de.sciss.lucre.{Disposable, Exec, Ident, Identified, Mutable, TOrdering, Var}
import de.sciss.serial.impl.ByteArrayOutputStream
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable, WritableFormat}

import scala.annotation.{elidable, switch, tailrec}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable.{PriorityQueue => MPriorityQueue, Queue => MQueue}

/** A transactional deterministic skip octree as outlined in the paper by Eppstein et al.
 * It is constructed from a given space (dimensions) and a skip-gap parameter
 * which determines the kind of skip list which is used to govern the
 * level decimation.
 *
 * The tree is a mutable data structure which supports lookup, insertion and removal
 * in O(log n), as well as efficient range queries and nearest neighbour search.
 *
 * The current implementation, backed by `impl.SkipOctreeImpl`, uses the types of
 * the `geom` package, assuming that coordinates are integers, with the maximum
 * root hyper-cube given by a span from `0` to `0x7FFFFFFF` (e.g. in `Space.IntTwoDim`,
 * this is `IntSquare( 0x40000000, 0x40000000, 0x40000000 )`.
 */
object DetSkipOctree {
  private final val SER_VERSION = 79  // 'O'

  private var stat_rounds = 0
  private var stat_pq_add = 0
  private var stat_pq_rem = 0
  private val stat_print  = false

  @volatile private var sanitizing = false

  @elidable(elidable.CONFIG) private def stat_reset(): Unit = {
    stat_rounds = 0
    stat_pq_add = 0
    stat_pq_rem = 0
  }

  @elidable(elidable.CONFIG) private def stat_report(): Unit = ()

  @elidable(elidable.CONFIG) private def stat_rounds1(obj: Any): Unit = {
    stat_rounds += 1
    if (stat_print) println(s"<stat> round max: $obj")
  }

  @elidable(elidable.CONFIG) private def stat_pq_add1(obj: Any): Unit = {
    stat_pq_add += 1
    if (stat_print) println(s"<stat> add    pq: $obj")
  }

  @elidable(elidable.CONFIG) private def stat_pq_rem1(obj: Any): Unit = {
    stat_pq_rem += 1
    if (stat_print) println(s"<stat> remove pq: $obj")
  }

  def empty[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](hyperCube: H, skipGap: Int = 2)
                                                          (implicit view: (A, T) => PL, tx: T, space: Space[PL, P, H],
                                                           keyFormat: TFormat[T, A]): DetSkipOctree[T, PL, P, H, A] =
    new ImplNew[T, PL, P, H, A](skipGap, tx.newId(), hyperCube, view, tx)

  def read[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](in: DataInput)(implicit tx: T,
                                                                         pointView: (A, T) => PL,
                                                                         space: Space[PL, P, H],
                                                                         keyFormat: TFormat[T, A]): DetSkipOctree[T, PL, P, H, A] = {
    val _pointView      = pointView
    val _keyFormat      = keyFormat
    val _tx: tx.type    = tx
    val _space          = space

//    new ImplRead[T, PL, P, H, A](view, in, tx)

//    private final class ImplRead[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A](val pointView: (A /*, T*/) => PL,
//                                                                                in: DataInput,
//                                                                                val tx: T)
//                                                                               (implicit val space: Space[PL, P, H],
//                                                                                val keyFormat: TFormat[T, A])

    new Impl[T, PL, P, H, A] {
      val pointView: (A, T) => PL   = _pointView
      val keyFormat: TFormat[T, A]  = _keyFormat
      val space: Space[PL, P, H]    = _space

      {
        val version = in.readByte()
        require(version == SER_VERSION,
          s"Incompatible serialized version (found $version, required $SER_VERSION).")
      }

      val id: Ident[T] = _tx.readId(in)
      val hyperCube: H = space.hyperCubeFormat.read(in) // (in, tx)
      val skipList: HASkipList.Set[T, this.Leaf] = {
        implicit val ord: TOrdering [T, this.Leaf] = LeafOrdering
        implicit val r1 : TFormat   [T, this.Leaf] = LeafFormat
        HASkipList.Set.format[T, this.Leaf](KeyObserver).readT(in)(_tx)
      }
      val headTree: this.LeftTopBranch = LeftTopBranchFormat.readT(in)(_tx)
      val lastTreeRef: Var[T, this.TopBranch] = {
        implicit val r4: TFormat[T, this.TopBranch] = TopBranchFormat
        id.readVar[this.TopBranch](in)
      }
    }
  }

  implicit def format[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](implicit view: (A, T) => PL, space: Space[PL, P, H],
                              keyFormat: TFormat[T, A]): TFormat[T, DetSkipOctree[T, PL, P, H, A]] =
    new OctreeFormat[T, PL, P, H, A]

  private final class OctreeFormat[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](implicit view: (A, T) => PL, space: Space[PL, P, H],
                              keyFormat: TFormat[T, A])
    extends WritableFormat[T, DetSkipOctree[T, PL, P, H, A]] {

    override def readT(in: DataInput)(implicit tx: T): DetSkipOctree[T, PL, P, H, A] =
      DetSkipOctree.read[T, PL, P, H, A](in)

    override def toString = "DetSkipOctree.format"
  }

  private final class ImplNew[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](skipGap: Int, val id: Ident[T], val hyperCube: H, val pointView: (A, T) => PL, tx0: T)
                             (implicit val space: Space[PL, P, H], val keyFormat: TFormat[T, A])
    extends Impl[T, PL, P, H, A] { octree =>

    val skipList: HASkipList.Set[T, this.Leaf] =
      HASkipList.Set.empty[T, this.Leaf](skipGap, KeyObserver)(tx0, LeafOrdering, LeafFormat)

    val headTree: this.LeftTopBranch = {
      val sz  = numOrthants
      val ch  = new Array[Var[T, this.LeftChild]](sz)
      val cid = tx0.newId()
      implicit val tx: T = tx0
      implicit val r1: TFormat[T, this.LeftChild] = LeftChildFormat
      var i = 0
      while (i < sz) {
        ch(i) = cid.newVar[this.LeftChild](Empty)
        i += 1
      }
      implicit val r2: TFormat[T, this.Next] = RightOptionReader
      val headRight = cid.newVar[this.Next](Empty)
      new LeftTopBranchImpl(octree, cid, children = ch, nextRef = headRight)
    }

    val lastTreeRef: Var[T, this.TopBranch] = {
      implicit val r3: TFormat[T, this.TopBranch] = TopBranchFormat
      implicit val tx: T = tx0
      id.newVar[this.TopBranch](headTree)
    }
  }

  sealed trait Child[+T, +PL, +P, +H, +A] extends Writable

  sealed trait LeftNonEmpty[T <: Exec[T], PL, P, H] extends Left with NonEmpty[T, PL, P, H]

  /** Utility trait which elements the rightward search `findPN`. */
  sealed trait ChildBranch[T <: Exec[T], PL, P, H, A]
    extends Branch       [T, PL, P, H, A] /* Like */
      with  NonEmptyChild[T, PL, P, H, A]

  sealed trait Next[+T, +PL, +P, +H, +A] extends Child[T, PL, P, H, A]

  /** A node is an object that can be stored in a orthant of a branch. */
  sealed trait NonEmpty[T <: Exec[T], PL, P, H]
    extends Identified[T] /* extends Down with Child */ {

    protected def shortString: String

    override def toString: String = s"$shortString$id"

    //    override def equals(that: Any): Boolean = that match {
    //      case n: Identifiable[_] => id == n.id   // do _not_ match against n: NonEmpty because that's an inner class!!!
    //      case _                  => super.equals(that)
    //    }
    //
    //    override def hashCode: Int = id.hashCode()

    /** Computes the greatest interesting hyper-cube within
     * a given hyper-cube `mq` so that this (leaf's or node's)
     * hyper-cube and the given point will be placed in
     * separated orthants of this resulting hyper-cube.
     */
    def union(mq: H, point: PL)(implicit tx: T): H

    /**
     * Queries the orthant index for this (leaf's or node's) hyper-cube
     * with respect to a given outer hyper-cube `iq`.
     */
    def orthantIndexIn(iq: H)(implicit tx: T): Int
  }

  sealed trait Left
  sealed trait LeftChild[+T, +PL, +P, +H, +A] extends Left with Child[T, PL, P, H, A]

  sealed trait Branch[T <: Exec[T], PL, P, H, A] extends Child[T, PL, P, H, A] with NonEmpty[T, PL, P, H] {
    /** Returns the hyper-cube covered by this node. */
    def hyperCube: H

    def nextOption(implicit tx: T): Option[Branch[T, PL, P, H, A]]

    /** Returns the corresponding interesting
     * node in Qi+1, or `empty` if no such
     * node exists.
     */
    def next(implicit tx: T): Next[T, PL, P, H, A]

    /** Sets the corresponding interesting
     * node in Qi+1.
     */
    private[DetSkipOctree] def next_=(n: Next[T, PL, P, H, A])(implicit tx: T): Unit

    def prevOption: Option[Branch[T, PL, P, H, A]]

    /** Returns the child for a given orthant index. */
    def child(idx: Int)(implicit tx: T): Child[T, PL, P, H, A]

    /** Assuming that the given `leaf` is a child of this node,
     * removes the child from this node's children. This method
     * will perform further clean-up such as merging this node
     * with its parent if it becomes uninteresting as part of the
     * removal.
     */
    private[DetSkipOctree] def demoteLeaf(point: PL, leaf: Leaf[T, PL, P, H, A])(implicit tx: T): Unit
  }

  sealed trait Leaf[T <: Exec[T], PL, P, H, A]
    extends Child             [T, PL, P, H, A]
      with  LeftNonEmptyChild [T, PL, P, H, A]
      with  RightNonEmptyChild[T, PL, P, H, A]
      with  LeafOrEmpty       [T, PL, P, H, A] {

    def value: A

    private[DetSkipOctree] def parent_=(b: Branch[T, PL, P, H, A])(implicit tx: T): Unit

    private[DetSkipOctree] def remove()(implicit tx: T): Unit
  }

  /** A common trait used in pattern matching, comprised of `Leaf` and `LeftChildBranch`. */
  sealed trait LeftNonEmptyChild[T <: Exec[T], PL, P, H, A]
    extends LeftNonEmpty [T, PL, P, H]
      with  NonEmptyChild[T, PL, P, H, A]
      with  LeftChild    [T, PL, P, H, A] with Writable {

    private[DetSkipOctree] def updateParentLeft(p: LeftBranch[T, PL, P, H, A])(implicit tx: T): Unit
  }

  sealed trait RightChild[+T, +PL, +P, +H, +A] extends Child[T, PL, P, H, A]

  /** An inner non empty tree element has a mutable parent node. */
  sealed trait NonEmptyChild[T <: Exec[T], PL, P, H, A] extends NonEmpty[T, PL, P, H] with Child[T, PL, P, H, A] {
    def parent(implicit tx: T): Branch[T, PL, P, H, A]
  }

  protected sealed trait LeafOrEmpty[+T, +PL, +P, +H, +A] extends LeftChild[T, PL, P, H, A]

  /** A common trait used in pattern matching, comprised of `Leaf` and `RightChildBranch`. */
  sealed trait RightNonEmptyChild[T <: Exec[T], PL, P, H, A]
    extends RightChild   [T, PL, P, H, A]
      with  NonEmptyChild[T, PL, P, H, A] with Writable {

    private[DetSkipOctree] def updateParentRight(p: RightBranch[T, PL, P, H, A])(implicit tx: T): Unit
  }

  sealed trait TopBranch[T <: Exec[T], PL, P, H, A] extends Branch[T, PL, P, H, A]

  sealed trait LeftTopBranch[T <: Exec[T], PL, P, H, A]
    extends LeftBranch  [T, PL, P, H, A]
      with  TopBranch   [T, PL, P, H, A]
      with  Disposable  [T]

  sealed trait RightTopBranch[T <: Exec[T], PL, P, H, A]
    extends RightBranch [T, PL, P, H, A]
      with  TopBranch   [T, PL, P, H, A]

  case object Empty
    extends LeftChild   [Nothing, Nothing, Nothing, Nothing, Nothing]
      with  RightChild  [Nothing, Nothing, Nothing, Nothing, Nothing]
      with  Next        [Nothing, Nothing, Nothing, Nothing, Nothing]
      with  LeafOrEmpty [Nothing, Nothing, Nothing, Nothing, Nothing] {

    def write(out: DataOutput): Unit = out.writeByte(0)
  }

  /** A left tree node implementation provides more specialized child nodes
   * of type `LeftChild`. It furthermore defines a resolution method
   * `findImmediateLeaf` which is typically called after arriving here
   * from a `findP0` call.
   */
  sealed trait LeftBranch[T <: Exec[T], PL, P, H, A]
    extends Branch[T, PL, P, H, A] /* Like */
      with LeftNonEmpty[T, PL, P, H] {

    def prevOption: Option[Branch[T, PL, P, H, A]]

    def child(idx: Int)(implicit tx: T): LeftChild[T, PL, P, H, A]

    private[DetSkipOctree] def insert(point: PL, value: A)(implicit tx: T): Leaf[T, PL, P, H, A]

    private[DetSkipOctree] def updateChild(idx: Int, c: LeftChild[T, PL, P, H, A])(implicit tx: T): Unit

    /** Instantiates an appropriate
     * leaf whose parent is this node, and which should be
     * ordered according to its position in this node.
     *
     * @param   qIdx  the orthant index of the new leaf in this node
     * @param   value the value associated with the new leaf
     * @return  the new leaf which has already assigned this node as
     *          parent and is already stored in this node's children
     *          at index `qIdx`
     */
    private[DetSkipOctree] def newLeaf(qIdx: Int, value: A)(implicit tx: T): Leaf[T, PL, P, H, A]
  }


  /** A right tree node implementation provides more specialized child nodes
   * of type `RightChild`. It furthermore defines the node in Qi-1 via the
   * `prev` method.
   */
  sealed trait RightBranch[T <: Exec[T], PL, P, H, A] extends Next[T, PL, P, H, A] with Branch[T, PL, P, H, A] {
    def prev: Branch[T, PL, P, H, A]

    private[DetSkipOctree] def updateChild(idx: Int, c: RightChild[T, PL, P, H, A])(implicit tx: T): Unit

    /** Promotes a leaf that exists in Qi-1 to this
     * tree, by inserting it into this node which
     * is its interesting node in Qi.
     *
     * If the result of insertion is a new child node
     * below this node, this intermediate node will
     * be connected to Qi by looking for the corresponding
     * hyper-cube in the given search path that led here
     * (i.e. that was constructed in `findPN`).
     *
     * This method also sets the parent of the leaf
     * accordingly.
     */
    private[DetSkipOctree] def insert(point: PL, leaf: Leaf[T, PL, P, H, A])(implicit tx: T): Unit
  }

  sealed trait LeftChildBranch[T <: Exec[T], PL, P, H, A]
    extends LeftBranch        [T, PL, P, H, A]
      with  ChildBranch       [T, PL, P, H, A]
      with  LeftNonEmptyChild [T, PL, P, H, A] {

    def parent(implicit tx: T): LeftBranch[T, PL, P, H, A]

    private[DetSkipOctree] def parent_=(node: LeftBranch[T, PL, P, H, A])(implicit tx: T): Unit
  }

  sealed trait RightChildBranch[T <: Exec[T], PL, P, H, A]
    extends RightBranch       [T, PL, P, H, A]
      with  ChildBranch       [T, PL, P, H, A]
      with  RightNonEmptyChild[T, PL, P, H, A] {

    def parent(implicit tx: T): RightBranch[T, PL, P, H, A]

    private[DetSkipOctree] def parent_=(node: RightBranch[T, PL, P, H, A])(implicit tx: T): Unit
  }

  /* Nodes are defined by a hyperCube area as well as a list of children,
   * as well as a pointer `next` to the corresponding node in the
   * next highest tree. A `Branch` also provides various search methods.
   */
  private sealed trait BranchImpl[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A] {
    thisBranch: Branch[T, PL, P, H, A] =>

    // ---- abstract ----

    protected def nextRef: Var[T, Next[T, PL, P, H, A]]

    /** Called when a leaf has been removed from the node.
     * The node may need to cleanup after this, e.g. promote
     * an under-full node upwards.
     */
    protected def leafRemoved()(implicit tx: T): Unit

    protected def nodeName: String

    // ---- impl ----

    final def next_=(node: Next[T, PL, P, H, A])(implicit tx: T): Unit = nextRef() = node

    final def next(implicit tx: T): Next[T, PL, P, H, A] = nextRef()

    final def nextOption(implicit tx: T): Option[Branch[T, PL, P, H, A]] = thisBranch.next match {
      case Empty                      => None
      case b: Branch[T, PL, P, H, A]  => Some(b)
    }

    final def union(mq: H, point2: PL)(implicit tx: T): H = {  // scalac warning bug
      val q = thisBranch.hyperCube
      mq.greatestInterestingH(q, point2)
    }

    final def orthantIndexIn(iq: H)(implicit tx: T): Int =  // scalac warning bug
      iq.indexOfH(thisBranch.hyperCube)

    protected final def shortString = s"$nodeName($thisBranch.hyperCube)"
  }

  private trait LeftBranchImpl[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A]
    extends BranchImpl[T, PL, P, H, A] {

    branch: LeftBranch[T, PL, P, H, A] =>

    // ---- abstract ----
    
    protected val octree: Impl[T, PL, P, H, A]

    /** For a `LeftBranch`, all its children are more specific
     * -- they are instances of `LeftChild` and thus support
     * order intervals.
     */
    protected def children: Array[Var[T, LeftChild[T, PL, P, H, A]]]

    // ---- impl ----

    final def prevOption: Option[Branch[T, PL, P, H, A]] = None

    final def child(idx: Int)(implicit tx: T): LeftChild[T, PL, P, H, A] = children(idx)()

    final def updateChild(idx: Int, c: LeftChild[T, PL, P, H, A])(implicit tx: T): Unit =
      children(idx)() = c

    final def demoteLeaf(point: PL, leaf: Leaf[T, PL, P, H, A])(implicit tx: T): Unit = {
      val qIdx  = branch.hyperCube.indexOfP(point)
      val ok    = child(qIdx) == leaf
      if (ok) {
        updateChild(qIdx, Empty)
        leafRemoved()
        leaf.remove() // dispose()
      } else {
        if (!DetSkipOctree.sanitizing)
          assert(assertion = false, s"Internal error - expected $leaf not found in $this")
      }
    }

    final def insert(point: PL, value: A)(implicit tx: T): Leaf[T, PL, P, H, A] = {
      val qIdx = branch.hyperCube.indexOfP(point)
      child(qIdx) match {
        case Empty =>
          newLeaf(qIdx, /* point, */ value) // (this adds it to the children!)

        case old: LeftNonEmptyChild[T, PL, P, H, A] =>
          // define the greatest interesting square for the new node to insert
          // in this node at qIdx:
          val qn2 = old.union(branch.hyperCube.orthant(qIdx), point)
          // create the new node (this adds it to the children!)
          val n2 = newNode(qIdx, qn2)
          val oIdx = old.orthantIndexIn(qn2)
          n2.updateChild(oIdx, old)
          val lIdx = qn2.indexOfP(point)
          assert(oIdx != lIdx)
          // This is a tricky bit! And a reason
          // why should eventually try to do without
          // parent pointers at all. Since `old`
          // may be a leaf whose parent points
          // to a higher level tree, we need to
          // check first if the parent is `this`,
          // and if so, adjust the parent to point
          // to the new intermediate node `ne`!
          if (old.parent == this) old.updateParentLeft(n2)
          n2.newLeaf(lIdx, value)
      }
    }

    /** Instantiates an appropriate
     * leaf whose parent is this node, and which should be
     * ordered according to its position in this node.
     *
     * @param   qIdx  the orthant index of the new leaf in this node
     * @param   value the value associated with the new leaf
     * @return  the new leaf which has already assigned this node as
     *          parent and is already stored in this node's children
     *          at index `qIdx`
     */
    private[DetSkipOctree] def newLeaf(qIdx: Int, value: A)(implicit tx: T): Leaf[T, PL, P, H, A] = {
      val leafId    = tx.newId()
      val parentRef = leafId.newVar[Branch[T, PL, P, H, A]](this)(tx, octree.BranchFormat)
      val l         = new LeafImpl[T, PL, P, H, A](octree, leafId, value, parentRef)
      updateChild(qIdx, l)
      l
    }

    /*
     * Instantiates an appropriate
     * sub-node whose parent is this node, and which should be
     * ordered according to its position in this node.
     *
     * @param   qIdx  the orthant index of the new node in this (parent) node
     * @param   iq    the hyper-cube of the new node
     * @return  the new node which has already assigned this node as
     *          parent and is already stored in this node's children
     *          at index `qIdx`
     */
    private[this] def newNode(qIdx: Int, iq: H)(implicit tx: T): LeftChildBranch[T, PL, P, H, A] = {
      val sz  = children.length
      val ch  = new Array[Var[T, LeftChild[T, PL, P, H, A]]](sz)
      val cid = tx.newId()
      var i = 0
      while (i < sz) {
        ch(i) = cid.newVar[LeftChild[T, PL, P, H, A]](Empty)(tx, octree.LeftChildFormat)
        i += 1
      }
      val parentRef   = cid.newVar[LeftBranch[T, PL, P, H, A]](this)(tx, octree.LeftBranchFormat)
      val rightRef    = cid.newVar[Next[T, PL, P, H, A]](Empty)(tx, octree.RightOptionReader)
      val n           = new LeftChildBranchImpl[T, PL, P, H, A](
        octree, cid, parentRef, iq, children = ch, nextRef = rightRef
      )
      updateChild(qIdx, n)
      n
    }
  }

  /* A leaf in the octree, carrying a map entry
   * in the form of a point and associated value.
   * Note that a single instance of a leaf is used
   * across the levels of the octree! That means
   * that multiple child pointers may go to the
   * same leaf, while the parent of a leaf always
   * points into the highest level octree that
   * the leaf resides in, according to the skiplist.
   */
  private final class LeafImpl[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](octree: Impl[T, PL, P, H, A], val id: Ident[T], val value: A,
                              parentRef: Var[T, Branch[T, PL, P, H, A]])
    extends LeftNonEmptyChild [T, PL, P, H, A] 
      with RightNonEmptyChild [T, PL, P, H, A]
      with LeafOrEmpty        [T, PL, P, H, A]
      with Leaf               [T, PL, P, H, A] {

    def updateParentLeft (p: LeftBranch [T, PL, P, H, A])(implicit tx: T): Unit = parent_=(p)
    def updateParentRight(p: RightBranch[T, PL, P, H, A])(implicit tx: T): Unit = parent_=(p)

    def parent(implicit tx: T): Branch[T, PL, P, H, A] = parentRef()

    def parent_=(p: Branch[T, PL, P, H, A])(implicit tx: T): Unit = parentRef() = p

    def dispose()(implicit tx: T): Unit = {
      id.dispose()
      parentRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(1)
      id.write(out)
      octree.keyFormat.write(value, out)
      parentRef.write(out)
    }

    def union(mq: H, point2: PL)(implicit tx: T): H =
      mq.greatestInterestingP(octree.pointView(value, tx), point2)

    def orthantIndexIn(iq: H)(implicit tx: T): Int =
      iq.indexOfP(octree.pointView(value, tx))

    def shortString = s"Leaf($value)"

    def remove()(implicit tx: T): Unit = dispose()
  }

  private final class LeftChildBranchImpl[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](val octree: Impl[T, PL, P, H, A], val id: Ident[T],
                              parentRef: Var[T, LeftBranch[T, PL, P, H, A]], val hyperCube: H,
                              protected val children: Array[Var[T, LeftChild[T, PL, P, H, A]]],
                              protected val nextRef: Var[T, Next[T, PL, P, H, A]])
    extends LeftBranchImpl[T, PL, P, H, A] 
      with LeftChildBranch[T, PL, P, H, A] {

    thisBranch =>

    protected def nodeName = "LeftInner"

    def updateParentLeft(p: LeftBranch[T, PL, P, H, A])(implicit tx: T): Unit = parent = p

    def parent(implicit tx: T): LeftBranch[T, PL, P, H, A] = parentRef()

    def parent_=(node: LeftBranch[T, PL, P, H, A])(implicit tx: T): Unit = parentRef() = node

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      parentRef .dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(3)
      id.write(out)
      parentRef.write(out)
      octree.space.hyperCubeFormat.write(thisBranch.hyperCube, out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    private[this] def remove()(implicit tx: T): Unit = dispose()

    // make sure the node is not becoming uninteresting, in which case
    // we need to merge upwards
    protected def leafRemoved()(implicit tx: T): Unit = {
      val sz = children.length
      @tailrec def removeIfLonely(i: Int): Unit =
        if (i < sz) child(i) match {
          case lonely: LeftNonEmptyChild[T, PL, P, H, A] =>
            @tailrec def isLonely(j: Int): Boolean = {
              j == sz || (child(j) match {
                case _: LeftNonEmptyChild[T, PL, P, H, A] => false
                case _ => isLonely(j + 1)
              })
            }
            if (isLonely(i + 1)) {
              val p     = parent
              val myIdx = p.hyperCube.indexOfH(thisBranch.hyperCube)
              p.updateChild(myIdx, lonely)
              if (lonely.parent == this) lonely.updateParentLeft(p)
              remove() // dispose() // removeAndDispose()
            }

          case _ => removeIfLonely(i + 1)
        }

      removeIfLonely(0)
    }
  }


  private trait RightBranchImpl[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A]
    extends BranchImpl[T, PL, P, H, A] {

    branch: RightBranch[T, PL, P, H, A] =>

    // ---- abstract ----
    
    protected val octree: Impl[T, PL, P, H, A]

    protected def children: Array[Var[T, RightChild[T, PL, P, H, A]]]

    // ---- impl ----

    final def prevOption: Option[Branch[T, PL, P, H, A]] = Some(prev: Branch[T, PL, P, H, A])

    final def child(idx: Int)(implicit tx: T): RightChild[T, PL, P, H, A] = children(idx)()

    final def updateChild(idx: Int, c: RightChild[T, PL, P, H, A])(implicit tx: T): Unit =
      children(idx)() = c

    /** Promotes a leaf that exists in Qi-1 to this
     * tree, by inserting it into this node which
     * is its interesting node in Qi.
     *
     * If the result of insertion is a new child node
     * below this node, this intermediate node will
     * be connected to Qi by looking for the corresponding
     * hyper-cube in the given search path that led here
     * (i.e. that was constructed in `findPN`).
     *
     * This method also sets the parent of the leaf
     * accordingly.
     */
    final def insert(point: PL, leaf: Leaf[T, PL, P, H, A])(implicit tx: T): Unit = {
      val qIdx = branch.hyperCube.indexOfP(point)
      child(qIdx) match {
        case Empty =>
          updateChild(qIdx, leaf)
          leaf.parent = this
        case old: RightNonEmptyChild[T, PL, P, H, A] =>
          // determine the greatest interesting square for the new
          // intermediate node to create
          val qn2 = old.union(branch.hyperCube.orthant(qIdx), point)
          // find the corresponding node in the lower tree
          @tailrec def findInPrev(b: Branch[T, PL, P, H, A]): Branch[T, PL, P, H, A] = {
            if (b.hyperCube == qn2) b
            else {
              val idx = b.hyperCube.indexOfP(point)
              b.child(idx) match {
                case _: LeafOrEmpty[T, PL, P, H, A] => sys.error("Internal error - cannot find sub-cube in prev")
                case cb: Branch[T, PL, P, H, A]     => findInPrev(cb)
              }
            }
          }
          val pPrev = findInPrev(prev)
          val n2    = newNode(qIdx, pPrev, qn2)
          val oIdx  = old.orthantIndexIn(qn2)
          n2.updateChild(oIdx, old)
          // This is a tricky bit! And a reason
          // why should eventually try to do without
          // parent pointers at all. Since `old`
          // may be a leaf whose parent points
          // to a higher level tree, we need to
          // check first if the parent is `this`,
          // and if so, adjust the parent to point
          // to the new intermediate node `ne`!
          if (old.parent == this) old.updateParentRight(n2)
          val lIdx = qn2.indexOfP(point)
          n2.updateChild(lIdx, leaf)
          leaf.parent = n2
      }
    }

    /*
     * Instantiates an appropriate
     * sub-node whose parent is this node, and whose predecessor
     * in the lower octree is given.
     *
     * @param   qIdx  the orthant index in this node where the node belongs
     * @param   prev  the new node's prev field, i.e. its correspondent in
     *                Qi-1
     * @param   iq    the hyper-cube for the new node
     * @return  the new node which has already been inserted into this node's
     *          children at index `qIdx`.
     */
    @inline private[this] def newNode(qIdx: Int, prev: Branch[T, PL, P, H, A],
                                      iq: H)(implicit tx: T): RightChildBranch[T, PL, P, H, A] = {
      val sz  = children.length
      val ch  = new Array[Var[T, RightChild[T, PL, P, H, A]]](sz)
      val cid = tx.newId()
      var i = 0
      implicit val format: TFormat[T, RightChild[T, PL, P, H, A]] = octree.RightChildFormat
      while (i < sz) {
        ch(i) = cid.newVar[RightChild[T, PL, P, H, A]](Empty)
        i += 1
      }
      val parentRef = cid.newVar[RightBranch[T, PL, P, H, A]](this)(tx, octree.RightBranchFormat)
      val rightRef  = cid.newVar[Next[T, PL, P, H, A]](Empty)(tx, octree.RightOptionReader)
      val n         = new RightChildBranchImpl[T, PL, P, H, A](octree, cid, parentRef, prev, iq, ch, rightRef)
      prev.next     = n
      updateChild(qIdx, n)
      n
    }

    final def demoteLeaf(point: PL, leaf: Leaf[T, PL, P, H, A])(implicit tx: T): Unit = {
      val qIdx = branch.hyperCube.indexOfP(point)
      assert(child(qIdx) == leaf)
      updateChild(qIdx, Empty)

      @tailrec def findParent(b: Branch[T, PL, P, H, A], idx: Int): Branch[T, PL, P, H, A] = b.child(idx) match {
        case sl: Leaf  [T, PL, P, H, A] => assert(sl == leaf); b
        case cb: Branch[T, PL, P, H, A] => findParent(cb, cb.hyperCube.indexOfP(point))
        case Empty                      => throw new IllegalStateException  // should not happen by definition
      }

      val newParent = findParent(prev, qIdx)
      leafRemoved()
      leaf.parent = newParent
    }
  }

  private final class RightChildBranchImpl[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](val octree: Impl[T, PL, P, H, A], val id: Ident[T],
                              parentRef: Var[T, RightBranch[T, PL, P, H, A]], val prev: Branch[T, PL, P, H, A],
                              val hyperCube: H,
                              protected val children: Array[Var[T, RightChild[T, PL, P, H, A]]],
                              protected val nextRef: Var[T, Next[T, PL, P, H, A]])
    extends RightChildBranch[T, PL, P, H, A] with RightBranchImpl[T, PL, P, H, A] {

    thisBranch =>

    protected def nodeName = "RightInner"

    def updateParentRight(p: RightBranch[T, PL, P, H, A])(implicit tx: T): Unit =
      parent = p

    private[this] def remove()(implicit tx: T): Unit = {
      // first unlink
      prev.next = Empty
      dispose()
    }

    def dispose()(implicit tx: T): Unit = {
      id.dispose()
      //         // first unlink
      //         prev.next = Empty

      // then dispose refs
      parentRef.dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(5)
      id.write(out)
      parentRef.write(out)
      prev.write(out)
      octree.space.hyperCubeFormat.write(thisBranch.hyperCube, out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    def parent(implicit tx: T): RightBranch[T, PL, P, H, A] = parentRef()

    def parent_=(node: RightBranch[T, PL, P, H, A])(implicit tx: T): Unit = parentRef() = node

    // make sure the node is not becoming uninteresting, in which case
    // we need to merge upwards
    protected def leafRemoved()(implicit tx: T): Unit = {
      val sz = children.length
      @tailrec def removeIfLonely(i: Int): Unit =
        if (i < sz) child(i) match {
          case lonely: RightNonEmptyChild[T, PL, P, H, A] =>
            @tailrec def isLonely(j: Int): Boolean = {
              j == sz || (child(j) match {
                case _: RightNonEmptyChild[T, PL, P, H, A]  => false
                case _                                      => isLonely(j + 1)
              })
            }
            if (isLonely(i + 1)) {
              val p       = parent
              val myIdx   = p.hyperCube.indexOfH(thisBranch.hyperCube)
              p.updateChild(myIdx, lonely)
              if (lonely.parent == this) lonely.updateParentRight(p)
              remove()
            }

          case _ => removeIfLonely(i + 1)
        }

      removeIfLonely(0)
    }
  }

  private final class LeftTopBranchImpl[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](val octree: Impl[T, PL, P, H, A], val id: Ident[T],
                              protected val children: Array[Var[T, LeftChild[T, PL, P, H, A]]],
                              protected val nextRef: Var[T, Next[T, PL, P, H, A]])
    extends LeftTopBranch[T, PL, P, H, A]
      with LeftBranchImpl[T, PL, P, H, A] 
      with TopBranchImpl [T, PL, P, H, A] 
      with Mutable[T] {
    
    // that's alright, we don't need to do anything special here
    protected def leafRemoved()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = {
      id.dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(2)
      id.write(out)
      // no need to write the hyperCube?
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    protected def nodeName = "LeftTop"
  }


  private final class RightTopBranchImpl[T <: Exec[T], PL, P,
    H <: HyperCube[PL, H], A](val octree: Impl[T, PL, P, H, A], val id: Ident[T], val prev: TopBranch[T, PL, P, H, A],
                              protected val children: Array[Var[T, RightChild[T, PL, P, H, A]]],
                              protected val nextRef: Var[T, Next[T, PL, P, H, A]])
    extends RightTopBranch[T, PL, P, H, A]
      with RightBranchImpl[T, PL, P, H, A]
      with TopBranchImpl  [T, PL, P, H, A] {

    protected def nodeName = "RightTop"

    private[this] def remove()(implicit tx: T): Unit = {
      // first unlink
      assert(octree.lastTree == this)
      octree.lastTree = prev
      prev.next       = Empty
      dispose()
    }

    def dispose()(implicit tx: T): Unit = {
      id.dispose()
      //         // first unlink
      //         assert( lastTreeImpl == this )
      //         lastTreeImpl= prev
      //         prev.next   = Empty

      // then dispose refs
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(4)
      id.write(out)
      // no need to write the hypercube!
      prev.write(out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    // remove this node if it empty now and right-node tree
    protected def leafRemoved()(implicit tx: T): Unit = {
      if (next != Empty) return

      val sz = children.length
      var i = 0
      while (i < sz) {
        val c = child(i)
        if (c != Empty) return // node not empty, abort the check
        i += 1
      }

      // ok, we are the right most tree and the node is empty...
      remove()
    }
  }

  private sealed trait TopBranchImpl[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A] {
    protected val octree: Impl[T, PL, P, H, A]
    
    final def hyperCube: H = octree.hyperCube
  }

  // cf. https://github.com/lampepfl/dotty/issues/9844
  private abstract class Impl[T <: Exec[T], PL, P, H <: HyperCube[PL, H], A]
    extends DetSkipOctree[T, PL, P, H, A] {
    octree =>

    final type Child              = DetSkipOctree.Child             [T, PL, P, H, A]
    final type Branch             = DetSkipOctree.Branch            [T, PL, P, H, A]
    final type Leaf               = DetSkipOctree.Leaf              [T, PL, P, H, A]
    final type LeftBranch         = DetSkipOctree.LeftBranch        [T, PL, P, H, A]
    final type RightBranch        = DetSkipOctree.RightBranch       [T, PL, P, H, A]
    final type LeftChild          = DetSkipOctree.LeftChild         [T, PL, P, H, A]
    final type RightChild         = DetSkipOctree.RightChild        [T, PL, P, H, A]
    final type Next               = DetSkipOctree.Next              [T, PL, P, H, A]
    final type ChildBranch        = DetSkipOctree.ChildBranch       [T, PL, P, H, A]
    final type LeftChildBranch    = DetSkipOctree.LeftChildBranch   [T, PL, P, H, A]
    final type RightChildBranch   = DetSkipOctree.RightChildBranch  [T, PL, P, H, A]
    final type NonEmptyChild      = DetSkipOctree.NonEmptyChild     [T, PL, P, H, A]
    final type LeafOrEmpty        = DetSkipOctree.LeafOrEmpty       [T, PL, P, H, A]
    final type LeftNonEmptyChild  = DetSkipOctree.LeftNonEmptyChild [T, PL, P, H, A]
    final type RightNonEmptyChild = DetSkipOctree.RightNonEmptyChild[T, PL, P, H, A]
    final type TopBranch          = DetSkipOctree.TopBranch         [T, PL, P, H, A]
    final type LeftTopBranch      = DetSkipOctree.LeftTopBranch     [T, PL, P, H, A]
    final type RightTopBranch     = DetSkipOctree.RightTopBranch    [T, PL, P, H, A]

    // ---- abstract types and methods ----
    
    implicit def space: Space[PL, P, H]
    implicit def keyFormat: TFormat[T, A]

    protected def skipList: HASkipList.Set[T, Leaf]
    protected def lastTreeRef: Var[T, TopBranch]

    // ----

    override def toString = s"Octree-${space.dim}d$id"

    protected object LeafOrdering extends TOrdering[T, Leaf] {
      /** Leaves are ordered by the tree's in-order traversal,
       * where the quadrants I+II and III+IV can be thought
       * of as dummy nodes to make the octree binary. That is
       * to say, in a node, the child order corresponds to
       * their quadrant indices (I < II < III < IV).
       */
      def compare(a: Leaf, b: Leaf)(implicit tx: T): Int = {
        val pa = pointView(a.value, tx)
        val pb = pointView(b.value, tx)
        space.lexicalOrder.compare(pa, pb)
      }
    }

    implicit object RightBranchFormat extends WritableFormat[T, RightBranch] {
      override def readT(in: DataInput)(implicit tx: T): RightBranch = {
        val cookie = in.readByte()
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 4 => readRightTopBranch  (in, id)
          case 5 => readRightChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    implicit object BranchFormat extends WritableFormat[T, Branch] {
      override def readT(in: DataInput)(implicit tx: T): Branch = {
        val cookie = in.readByte()
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 2 => readLeftTopBranch   (in, id)
          case 3 => readLeftChildBranch (in, id)
          case 4 => readRightTopBranch  (in, id)
          case 5 => readRightChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    protected object TopBranchFormat extends WritableFormat[T, TopBranch] {
      override def readT(in: DataInput)(implicit tx: T): TopBranch = {
        val cookie = in.readByte()
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 2 => readLeftTopBranch (in, id)
          case 4 => readRightTopBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    object LeftChildFormat extends WritableFormat[T, LeftChild] {
      override def readT(in: DataInput)(implicit tx: T): LeftChild = {
        val cookie = in.readByte()
        if (cookie == 0) return Empty
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 1 => readLeaf(in, id)
          case 3 => readLeftChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    implicit object LeftBranchFormat extends WritableFormat[T, LeftBranch] {
      override def readT(in: DataInput)(implicit tx: T): LeftBranch = {
        val cookie = in.readByte()
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 2 => readLeftTopBranch  (in, id)
          case 3 => readLeftChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    implicit object RightChildFormat extends WritableFormat[T, RightChild] {
      override def readT(in: DataInput)(implicit tx: T): RightChild = {
        val cookie = in.readByte()
        if (cookie == 0) return Empty
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 1 => readLeaf(in, id)
          case 5 => readRightChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    implicit object LeftTopBranchFormat extends WritableFormat[T, LeftTopBranch] {
      override def readT(in: DataInput)(implicit tx: T): LeftTopBranch = {
        val cookie = in.readByte()
        if (cookie != 2) sys.error(s"Unexpected cookie $cookie")
        val id = tx.readId(in)
        readLeftTopBranch(in, id)
      }
    }

    object RightOptionReader extends WritableFormat[T, Next] {
      override def readT(in: DataInput)(implicit tx: T): Next = {
        val cookie = in.readByte()
        if (cookie == 0) return Empty
        val id = tx.readId(in)
        (cookie: @switch) match {
          case 4 => readRightTopBranch  (in, id)
          case 5 => readRightChildBranch(in, id)
          case _ => sys.error(s"Unexpected cookie $cookie")
        }
      }
    }

    protected object LeafFormat extends WritableFormat[T, Leaf] {
      override def readT(in: DataInput)(implicit tx: T): Leaf = {
        val cookie = in.readByte()
        if (cookie != 1) sys.error(s"Unexpected cookie $cookie")
        val id = tx.readId(in)
        readLeaf(in, id)
      }
    }

    implicit protected object KeyObserver extends SkipList.KeyObserver[T, Leaf] {
      def keyUp(l: Leaf)(implicit tx: T): Unit = {
        //println( "up : " + l )
        // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
        //  then traverse upwards in Qi until we find the lowest
        //  ancestor q of x which is also interesting in Qi+1.
        //  (This is the reversed process of searching x in Qi
        //  with q = pi,start = pi+1,end so it takes at most 6
        //  steps by Lemma 5.) Then we go to the same square q
        //  in Qi+1 and insert x."

        /* The reverse process of `findP0`: Finds the lowest
         * common ancestor interesting node of this node
         * which is also contained in Qi+1. Returns this node
         * in Qi+1, or empty if no such node exists.
         */
        @tailrec def findPN(b: Branch): Next = b match {
          case tb: TopBranch    => tb.next
          case cb: ChildBranch  => cb.next match {
            case nb: Branch     => nb
            case Empty          => findPN(cb.parent)
          }
        }

        val pNext = findPN(l.parent) match {
          case Empty => // create new level
            val sz  = numOrthants
//            val ch  = tx.newVarArray[RightChild](sz)
            val ch  = new Array[Var[T, RightChild]](sz)
            val cid = tx.newId()
            var i   = 0
            while (i < sz) {
              ch(i) = cid.newVar[RightChild](Empty)
              i += 1
            }
            val nextRef   = cid.newVar[Next](Empty)(tx, RightOptionReader)
            val prev      = lastTree
            val res       = new RightTopBranchImpl(octree, cid, prev, ch, nextRef)
            prev.next     = res
            lastTree  = res
            res
          case r: RightBranch => r
        }
        pNext.insert(pointView(l.value, tx), l)
      }

      def keyDown(l: Leaf)(implicit tx: T): Unit = {
        //println( "down : " + l )
        // "To delete x from Qi we go from xi to the smallest interesting
        //  square pi(x) containing x in Qi following the pointers. Then
        //  the deletion given pi(x) is as described in Section 2.3."

        l.parent.demoteLeaf(pointView(l.value, tx), l)
      }
    }

    final def numOrthants: Int = 1 << space.dim  // 4 for R2, 8 for R3, 16 for R4, etc.

    final def write(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      id          .write(out)
      space.hyperCubeFormat.write(hyperCube, out)
      skipList    .write(out)
      headTree    .write(out)
      lastTreeRef .write(out)
    }

    final def clear()(implicit tx: T): Unit = {
      val sz = numOrthants
      @tailrec def removeAllLeaves(b: Branch): Unit = {
        @tailrec def stepB(down: Branch, i: Int): Child = {
          if (i == sz) down
          else b.child(i) match {
            case l: Leaf =>
              removeLeaf(pointView(l.value, tx), l)
              lastTree
            case _ => stepB(down, i + 1)
          }
        }

        @tailrec def step(i: Int): Child = {
          if (i == sz) Empty
          else b.child(i) match {
            case cb: Branch => stepB(cb, i + 1)
            case l: Leaf =>
              removeLeaf(pointView(l.value, tx), l)
              lastTree
            case _ => step(i + 1)
          }
        }

        step(0) match {
          case _: LeafOrEmpty   =>
          case next: Branch => removeAllLeaves(next)
        }
      }
      removeAllLeaves(lastTree)
    }

    final def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      lastTreeRef .dispose()
      headTree    .dispose()
      skipList    .dispose()
    }

    final def lastTree(implicit tx: T): TopBranch = lastTreeRef()

    final def lastTree_=(node: TopBranch)(implicit tx: T): Unit = lastTreeRef() = node

    final def size(implicit tx: T): Int = skipList.size

    final def add(elem: A)(implicit tx: T): Boolean =
      insertLeaf(elem) match {
        case Empty          => true
        case oldLeaf: Leaf  => oldLeaf.value != elem
      }

    final def update(elem: A)(implicit tx: T): Option[A] =
      insertLeaf(elem) match {
        case Empty          => None
        case oldLeaf: Leaf  => Some(oldLeaf.value)
      }

    final def remove(elem: A)(implicit tx: T): Boolean =
      removeLeafAt(pointView(elem, tx)) != Empty

    final def removeAt(point: PL)(implicit tx: T): Option[A] =
      removeLeafAt(point) match {
        case Empty          => None
        case oldLeaf: Leaf  => Some(oldLeaf.value)
      }

    final def contains(elem: A)(implicit tx: T): Boolean = {
      val point = pointView(elem, tx)
      if (!hyperCube.containsP(point)) return false
      findAt(point) match {
        case l: Leaf  => l.value == elem
        case _        => false
      }
    }

    final def isDefinedAt(point: PL)(implicit tx: T): Boolean = {
      if (!hyperCube.containsP(point)) return false
      findAt(point) != Empty
    }

    final def get(point: PL)(implicit tx: T): Option[A] = {
      if (!hyperCube.containsP(point)) return None
      findAt(point) match {
        case l: Leaf  => Some(l.value)
        case _        => None
      }
    }

    final def nearestNeighbor[M](point: PL, metric: DistanceMeasure[M, PL, H])(implicit tx: T): A = {
      val nn = new NN(point, metric).find()
      stat_report()
      nn match {
        case Empty    => throw new NoSuchElementException("nearestNeighbor on an empty tree")
        case l: Leaf  => l.value
      }
    }

    final def nearestNeighborOption[M](point: PL, metric: DistanceMeasure[M, PL, H])(implicit tx: T): Option[A] = {
      val nn = new NN(point, metric).find()
      stat_report()
      nn match {
        case Empty    => None
        case l: Leaf  => Some(l.value)
      }
    }

    final def isEmpty(implicit tx: T): Boolean = {
      val n = headTree
      val sz = numOrthants
      @tailrec def step(i: Int): Boolean = if (i == sz) true
      else n.child(i) match {
        case _: NonEmptyChild => false
        case _ => step(i + 1)
      }
      step(0)
    }

    final def numLevels(implicit tx: T): Int = {
      @tailrec def step(b: Branch, num: Int): Int = {
        b.next match {
          case Empty => num
          case n: Branch => step(n, num + 1)
        }
      }
      step(headTree, 1)
    }

    final def +=(elem: A)(implicit tx: T): this.type = {
      insertLeaf(elem)
      //      match {
      //         case oldLeaf: Leaf => oldLeaf.dispose()
      //         case _ =>
      //      }
      this
    }

    final def -=(elem: A)(implicit tx: T): this.type = {
      removeLeafAt(pointView(elem, tx))
      //      match {
      //         case oldLeaf: Leaf => oldLeaf.dispose()
      //         case _ =>
      //      }
      this
    }

    final def rangeQuery[Area](qs: QueryShape[Area, PL, H])(implicit tx: T): Iterator[A] = {
      val q = new RangeQuery(qs)
      q.findNextValue()
      q
    }

    final def toIndexedSeq(implicit tx: T): Vec[A] = iterator.toIndexedSeq

    final def toList(implicit tx: T): List[A] = iterator.toList

    // note that `iterator.toSeq` produces a `Stream` !!
    final def toSeq(implicit tx: T): Seq[A] = iterator.toIndexedSeq

    final def toSet(implicit tx: T): Set[A] = iterator.toSet

    private[this] def findAt(point: PL)(implicit tx: T): LeafOrEmpty = {
      val p0 = findP0(point) // lastTreeImpl.findP0( point )
      findLeafInP0(p0, point) // p0.findImmediateLeaf( point )
    }

    // OBSOLETE: the caller _must not call dispose_
    //
    // (( WARNING: if the returned oldLeaf is defined, the caller is
    // responsible for disposing it (after extracting useful information such as its value) ))
    private[this] def insertLeaf(elem: A)(implicit tx: T): LeafOrEmpty = {
      val point = pointView(elem, tx)
      if (!hyperCube.containsP(point)) sys.error(s"$point lies out of root hyper-cube $hyperCube")

      val p0  = findP0(point) // lastTreeImpl.findP0( point )
      val res = findLeafInP0(p0, point)

      res match {
        case Empty =>
          val leaf = p0.insert(point, elem)
          skipList.add(leaf)

        case oldLeaf: Leaf =>
          // remove previous leaf
          removeLeaf(point, oldLeaf)
          // search anew
          val p0b = findP0(point) // lastTreeImpl.findP0( point )
          assert(findLeafInP0(p0b, point) == Empty)
          val leaf = p0b.insert(point, elem)
          skipList.add(leaf)
      }

      res
    }

    // WARNING: if the returned oldLeaf is defined, the caller is
    // responsible for disposing it (after extracting useful information such as its value)
    private[this] def removeLeafAt(point: PL)(implicit tx: T): LeafOrEmpty = {
      if (!hyperCube.containsP(point)) return Empty

      // "To insert or delete a point y into or from S, we first search the
      // quadtree structure to locate y in each Qi ..."
      val p0 = findP0(point) // lastTreeImpl.findP0( point )

      // "... Then we insert or delete y
      // in the binary Q0 and update our total order."

      val res = findLeafInP0(p0, point) // p0.findImmediateLeaf( point )

      res match {
        case l: Leaf  => removeLeaf(point, l)
        case _        =>
      }

      res
    }

    def transformAt(point: PL)(fun: Option[A] => Option[A])(implicit tx: T): Option[A] = {
      require(hyperCube.containsP(point), s"$point lies out of root hyper-cube $hyperCube")

      val p0 = findP0(point)
      findLeafInP0(p0, point) match {
        case Empty =>
          val res = None
          fun(res).foreach { elem =>
            val leaf = p0.insert(point, elem)
            skipList.add(leaf)
          }
          res

        case oldLeaf: Leaf =>
          // it's not possible currently to update a leaf's value...
          // remove previous leaf
          val res = Some(oldLeaf.value)
          removeLeaf(point, oldLeaf)
          fun(res).foreach {
            elem =>
              // search anew
              val p0b = findP0(point)
              assert(findLeafInP0(p0b, point) == Empty)
              val leaf = p0b.insert(point, elem)
              skipList.add(leaf)
          }
          res
      }
    }

    /*
     * After arriving at this node from a `findP0` call, this resolves
     * the given point to an actual leaf.
     *
     * @return  the `Leaf` child in this node associated with the given
     *          `point`, or `empty` if no such leaf exists.
     */
    private[this] def findLeafInP0(b: LeftBranch, point: PL)(implicit tx: T): LeafOrEmpty = {
      val qIdx = b.hyperCube.indexOfP(point)
      b.child(qIdx) match {
        case l: Leaf if pointView(l.value, tx) == point => l
        case _ => Empty
      }
    }

    /*
     * Finds to smallest interesting hyper-cube
     * in Q0, containing a given point. This method
     * traverses downwards into its children, or,
     * if the "bottom" has been reached, tries to
     * continue in Qi-1.
     *
     * @return  the node defined by the given search `point`, or `empty`
     *          if no such node exists.
     */
    private[this] def findP0(point: PL)(implicit tx: T): LeftBranch = {
      @tailrec def stepLeft(lb: LeftBranch): LeftBranch = {
        val qIdx = lb.hyperCube.indexOfP(point)
        lb.child(qIdx) match {
          case _: LeafOrEmpty => lb
          case cb: LeftBranch =>
            if (!cb.hyperCube.containsP(point)) lb else stepLeft(cb)
        }
      }

      @tailrec def step(b: Branch): LeftBranch = b match {
        case lb: LeftBranch => stepLeft(lb)
        case rb: RightBranch =>
          val qIdx = rb.hyperCube.indexOfP(point)
          val n = rb.child(qIdx) match {
            case cb: Branch if cb.hyperCube.containsP(point) => cb
            case _ => rb.prev
          }
          step(n)
      }

      step(lastTree)
    }

    private[this] def removeLeaf(point: PL, l: Leaf)(implicit tx: T): Unit = {
      // this will trigger removals from upper levels
      val skipOk = skipList.remove(l)
      assert(skipOk, s"Leaf $l with point $point was not found in skip list")
      // now l is in P0. demote it once more (this will dispose the leaf)
      l.parent.demoteLeaf(point /* pointView( l.value ) */ , l)
    }

    final def iterator(implicit tx: T): Iterator[A] = skipList.iterator.map(_.value)

    private[this] final class NNIteration[M](val bestLeaf: LeafOrEmpty, val bestDist: M, val rMax: M)

    private[this] final class NN[M](point: PL, metric: DistanceMeasure[M, PL, H])(implicit tx: T)
      extends scala.math.Ordering[VisitedNode[M]] {

      stat_reset()

      // NOTE: `sz` must be protected and not private, otherwise
      // scala's specialization blows up
      protected val sz: Int = numOrthants
      private[this] val acceptedChildren = new Array[Branch](sz)
      //      private val acceptedDistances   = {
      //         implicit val mf = metric.manifest
      //         new Array[ M ]( sz )
      //      }
      private[this] val acceptedMinDistances = metric.newArray(sz)
      // private val acceptedMaxDistances = metric.newArray(sz)

      /* @tailrec */ private[this] def findNNTailOLD(n0: /* Left */ Branch, pri: MPriorityQueue[VisitedNode[M]],
                                                     _bestLeaf: LeafOrEmpty, _bestDist: M, _rMax: M): NNIteration[M] = {
        stat_rounds1(_rMax)
        var numAccepted   = 0
        var acceptedQIdx  = 0

        var bestLeaf  = _bestLeaf
        var bestDist  = _bestDist
        var rMax      = _rMax

        var i = 0
        while (i < sz) {
          n0.child(i) match {
            case l: Leaf =>
              val lDist = metric.distance(point, pointView(l.value, tx))
              if (metric.isMeasureGreater(bestDist, lDist)) {   // found a point that is closer than previously known best result
                bestDist = lDist
                bestLeaf = l
                if (metric.isMeasureGreater(rMax, bestDist)) {  // update minimum required distance if necessary
                  rMax = bestDist // note: we'll re-check acceptedChildren at the end of the loop
                }
              }
            case c: LeftBranch =>
              val cq = c.hyperCube
              val cMinDist = metric.minDistance(point, cq)
              if (!metric.isMeasureGreater(cMinDist, rMax)) {   // is less than or equal to minimum required distance
                // (otherwise we're out already)
                val cMaxDist = metric.maxDistance(point, cq)
                if (metric.isMeasureGreater(rMax, cMaxDist)) {
                  rMax = cMaxDist                               // found a new minimum required distance
                }
                acceptedChildren(numAccepted) = c
                acceptedMinDistances(numAccepted) = cMinDist
                // acceptedMaxDistances(numAccepted) = cMaxDist
                numAccepted += 1
                acceptedQIdx = i                                // this will be used only if numAccepted == 1
              }
            case _ => // ignore empty orthants
          }
          i += 1
        }

        if (rMax != _rMax) {
          // recheck
          var j = 0
          while (j < numAccepted) {
            if (metric.isMeasureGreater(acceptedMinDistances(j), rMax)) {
              // immediately kick it out
              numAccepted -= 1
              var k = j
              while (k < numAccepted) {
                val k1 = k + 1
                acceptedChildren(k) = acceptedChildren(k1)
                acceptedMinDistances(k) = acceptedMinDistances(k1)
                // acceptedMaxDistances(k) = acceptedMaxDistances(k1)
                k = k1
              }
            }
            j += 1
          }
        }

        // Unless exactly one child is accepted, round is over
        //      if (numAccepted != 1) {
        /* var */ i = 0
        while (i < numAccepted) { // ...and the children are added to the priority queue
          val vn = new VisitedNode[M](acceptedChildren(i), acceptedMinDistances(i) /*, acceptedMaxDistances(i) */)
          stat_pq_add1(vn)
          pri += vn
          i += 1
        }
        new NNIteration[M](bestLeaf, bestDist, rMax)
      }

      def find(): LeafOrEmpty = {
        val pri = MPriorityQueue.empty[VisitedNode[M]](this)
        @tailrec def step(p0: Branch, pMinDist: M, bestLeaf: LeafOrEmpty, bestDist: M, rMax: M): LeafOrEmpty = {
          val res = findNNTailOLD(p0, /* pMinDist, */ pri, bestLeaf, bestDist, rMax)
          if (metric.isMeasureZero(res.bestDist)) {
            res.bestLeaf   // found a point exactly at the query position, so stop right away
          } else {
            if (pri.isEmpty) res.bestLeaf
            else {
              val vis = pri.dequeue()
              stat_pq_rem1(vis.n.hyperCube)
              // if (!metric.isMeasureGreater(vis.minDist, res.rMax)) vis.n else pop()

              // because the queue is sorted by smallest minDist, if we find an element
              // whose minimum distance is greater than the maximum distance allowed,
              // we are done and do not need to process the remainder of the priority queue.

              if (metric.isMeasureGreater(vis.minDist, res.rMax)) res.bestLeaf else {
                val lb = vis.n
                step(lb, vis.minDist, res.bestLeaf, res.bestDist, res.rMax)
              }
            }
          }
        }

        val mMax      = metric.maxValue
        val p         = headTree // lastTree
        val pMinDist  = metric.minDistance(point, octree.hyperCube)  // XXX could have metric.zero
        step(p, pMinDist, Empty, mMax, mMax)
      }

      def compare(a: VisitedNode[M], b: VisitedNode[M]): Int = {
        val min = metric.compareMeasure(b.minDist, a.minDist)
        min // if (min != 0) min else metric.compareMeasure(b.maxDist, a.maxDist)
      }
    }

    private[this] final class VisitedNode[M](val n: Branch, val minDist: M /*, val maxDist: M */) {
      override def toString = s"($n, min = $minDist" // , max = $maxDist)"
    }

    // note: Iterator is not specialized, hence we can safe use the effort to specialize in A anyway
    private[this] final class RangeQuery[Area](qs: QueryShape[Area, PL, H])(implicit tx: T) extends Iterator[A] {
      val sz: Int = numOrthants
      val stabbing: MQueue[(Branch, Area)] = MQueue.empty
      // Tuple2 is specialized for Long, too!
      val in: MQueue[NonEmptyChild] = MQueue.empty
      var current: A  = _
      // overwritten by initial run of `findNextValue`
      var hasNextVar  = true // eventually set to `false` by `findNextValue`

      stabbing += headTree -> qs.overlapArea(headTree.hyperCube)

      //      findNextValue()

      override def toString = s"$octree.rangeQuery($qs)"

      def hasNext: Boolean = hasNextVar

      // search downwards:
      // "At each square q ∈ Qi we either go to a child square in Qi
      // that covers the same area of R ∪ A as p does, if such a child
      // square exists, or jump to the next level q ∈ Qi−1."
      @tailrec private[this] def findEquidistantStabbingTail(node: Branch, area: Area): LeftBranch = {
        var pi = node
        var i = 0
        while (i < sz) {
          pi.child(i) match {
            case pic: Branch =>
              val a2 = qs.overlapArea(pic.hyperCube)
              if (a2 == area) {
                pi = pic
                i = 0 // start over in child
              } else {
                i += 1
              }
            case _ => i += 1
          }
        }
        // ... or jump to the next (previous) level
        pi match {
          case lb: LeftBranch => lb
          case rb: RightBranch => findEquidistantStabbingTail(rb.prev, area)
        }
      }

      // the movement from Q0 to Qj
      // "assuming that p is not critical in Q0, we promote to Qj where Qj is the highest
      // level in which p is not a critical square"
      //
      // definition of critical square:
      // "a stabbing node of Qi whose child nodes are either not stabbing, or still
      // stabbing but cover less volume of R than p does."
      // ; or rather inverted: an uncritical node is one, in which exists at least one stabbing node
      // with the same overlapping area!
      //
      // definition stabbing: 0 < overlap-area < area-of-p
      @tailrec def findHighestUncritical(p0: Branch, area: Area): Branch = {
        @tailrec def isCritical(b: Branch, i: Int): Boolean = {
          i < sz && (b.child(i) match {
            // if there is any child which has the same overlap area, it means the node is uncritical
            case ci: Branch if qs.overlapArea(ci.hyperCube) == area => true
            case _ => isCritical(b, i + 1)
          })
        }

        p0.next match {
          case Empty => p0
          case pi: Branch => if (isCritical(pi, 0)) p0 else findHighestUncritical(pi, area)
        }
      }

      def next(): A = {
        if (!hasNextVar) throw new java.util.NoSuchElementException("next on empty iterator")
        val res = current
        findNextValue()
        res
      }

      def findNextValue(): Unit = {
        while (true) {
          if (in.isEmpty) {
            if (stabbing.isEmpty) {
              hasNextVar = false
              return
            }
            val tup = stabbing.dequeue()
            val ns  = tup._1 // stabbing node
            val as  = tup._2 // overlapping area with query shape
            val hi  = findHighestUncritical(ns, as) // find highest uncritical hyper-cube of the stabbing node
            val nc  = findEquidistantStabbingTail (hi, as) // now traverse towards Q0 to find the critical square

            var i = 0
            while (i < sz) {
              nc.child(i) match {
                case cl: Leaf =>
                  if (qs.containsP(pointView(cl.value, tx))) in += cl
                case cn: ChildBranch =>
                  val q   = cn.hyperCube
                  val ao  = qs.overlapArea(q)
                  // test for stabbing or inclusion:
                  // inclusion: overlap-area == area-of-p
                  // stabbing: 0 < overlap-area < area-of-p
                  if (qs.isAreaNonEmpty(ao)) {
                    // q is _not_ out
                    if (qs.isAreaGreater(q, ao)) {
                      // q is stabbing
                      stabbing += cn -> ao
                    } else {
                      // q is in
                      in += cn
                    }
                  }
                case _ =>
              }
              i += 1
            }

          } else {
            in.dequeue() match {
              case l: Leaf =>
                current = l.value
                return
              case n: Branch =>
                var i = 0
                while (i < sz) {
                  n.child(i) match {
                    case cc: NonEmptyChild => in += cc // `enqueue` would create intermediate Seq because of varargs
                    case _ =>
                  }
                  i += 1
                }
            }
          }
        }
      }
    }

    /*
     * Serialization-id: 1
     */
    private[this] def readLeaf(in: DataInput, id: Ident[T])(implicit tx: T): Leaf = {
      val value     = keyFormat.readT(in)
      val parentRef = id.readVar[Branch](in)
      new LeafImpl(octree, id, value, parentRef)
    }
    
    /*
     * Serialization-id: 2
     */
    private[this] def readLeftTopBranch(in: DataInput, id: Ident[T])(implicit tx: T): LeftTopBranch = {
      val sz  = numOrthants
//      val ch  = tx.newVarArray[LeftChild](sz)
      val ch  = new Array[Var[T, LeftChild]](sz)
      var i = 0
      while (i < sz) {
        ch(i) = id.readVar[LeftChild](in)(tx, LeftChildFormat)
        i += 1
      }
      val nextRef = id.readVar[Next](in)(tx, RightOptionReader)
      new LeftTopBranchImpl(octree, id, children = ch, nextRef = nextRef)
    }

    /*
     * Serialization-id: 3
     */
    private[this] def readLeftChildBranch(in: DataInput, id: Ident[T])
                                         (implicit tx: T): LeftChildBranch = {
      val parentRef   = id.readVar[LeftBranch](in)
      val hc          = space.hyperCubeFormat.read(in)
      val sz          = numOrthants
//      val ch          = tx.newVarArray[LeftChild](sz)
      val ch          = new Array[Var[T, LeftChild]](sz)
      var i = 0
      while (i < sz) {
        ch(i) = id.readVar[LeftChild](in)(tx, LeftChildFormat)
        i += 1
      }
      val nextRef = id.readVar[Next](in)(tx, RightOptionReader)
      new LeftChildBranchImpl(octree, id, parentRef, hc, children = ch, nextRef = nextRef)
    }
    
    /*
      * Serialization-id: 4
      */
    private[this] def readRightTopBranch(in: DataInput, id: Ident[T])
                                        (implicit tx: T): RightTopBranch = {
      val prev  = TopBranchFormat.readT(in)
      val sz    = numOrthants
//      val ch    = tx.newVarArray[RightChild](sz)
      val ch    = new Array[Var[T, RightChild]](sz)
      var i = 0
      while (i < sz) {
        ch(i) = id.readVar[RightChild](in)
        i += 1
      }
      val nextRef = id.readVar[Next](in)(tx, RightOptionReader)
      new RightTopBranchImpl(octree, id, prev, ch, nextRef)
    }

    /*
      * Serialization-id: 5
      */
    private[this] def readRightChildBranch(in: DataInput, id: Ident[T])
                                          (implicit tx: T): RightChildBranch = {
      val parentRef = id.readVar[RightBranch](in)
      val prev      = BranchFormat.readT(in)
      val hc        = space.hyperCubeFormat.read(in)
      val sz        = numOrthants
      val ch        = new Array[Var[T, RightChild]](sz)
      var i = 0
      while (i < sz) {
        ch(i) = id.readVar[RightChild](in)
        i += 1
      }
      val nextRef = id.readVar[Next](in)(tx, RightOptionReader)
      new RightChildBranchImpl(octree, id, parentRef, prev, hc, ch, nextRef)
    }
    
    def debugPrint()(implicit tx: T): String = {
      val bs  = new ByteArrayOutputStream()
      val ps  = new PrintStream(bs)
      import ps._

      println(s"Debug print for $this")
      println("Skip list of leaves:")
      println(skipList.debugPrint())
      println("Octree structure:")

      def dumpTree(b: Branch, indent: Int): Unit = {
        val iStr = " " * indent
        b match {
          case lb: LeftBranch =>
            println(s"${iStr}LeftBranch${lb.id} with ${b.hyperCube}")
          case _ =>
            println(s"${iStr}RightBranch${b.id} with ${b.hyperCube}")
        }
        for(i <- 0 until numOrthants) {
          print(s"$iStr  Child #${i+1} = ")
          b.child(i) match {
            case cb: Branch =>
              println("Branch:")
              dumpTree(cb, indent + 4)
            case l: Leaf =>
              println(s"Leaf${l.id} ${l.value}")
            case empty => println(empty)
          }
        }
      }

      def dumpTrees(b: Branch, level: Int): Unit = {
        println(s"\n---level $level----")
        dumpTree(b, 0)
        b.nextOption.foreach(b => dumpTrees(b, level + 1))
      }

      dumpTrees(headTree, 0)
      ps.close()
      new String(bs.toByteArray, "UTF-8")
    }


    /* A debugging facility to inspect an octree only (not the skip list) for internal structural consistency.
     *
     * @param tree       the tree to inspect.
     * @param reportOnly if `true`, merely reports anomalies but does not try to resolve them. If `false` attempts to
     *                   fix corrupt entries.
     * @return           empty if there were no inconsistencies found, otherwise a list of textual descriptions
     *                   of the problems found
     */
    private def verifyOctreeConsistency(reportOnly: Boolean)(implicit tx: T): Vec[String] = {
      val q                   = hyperCube
      var level               = numLevels
      var h: Branch           = lastTree
      var currUnlinkedOcs     = Set.empty[H]
      var currPoints          = Set.empty[Leaf]
      var errors              = Vec.empty[String]
      val repair              = !reportOnly

      while ({
        if (h.hyperCube != q) {
          errors :+= s"Root level quad is ${h.hyperCube} while it should be $q in level $level"
        }
        val nextUnlinkedOcs = currUnlinkedOcs

        val nextPoints  = currPoints
        currUnlinkedOcs = Set.empty
        currPoints      = Set.empty

        def checkChildren(n: Branch, depth: Int): Unit = {
          def assertInfo = s"in level $level / depth $depth"

          var i = 0
          while (i < numOrthants) {
            n.child(i) match {
              case cb: ChildBranch =>
                if (cb.parent != n) {
                  errors :+= s"Child branch $cb has invalid parent ${cb.parent}, expected: $n $assertInfo"
                  if (repair) {
                    ((n, cb): @unchecked) match {
                      case (pl: LeftBranch , cbl: LeftChildBranch ) => cbl.parent = pl
                      case (pr: RightBranch, cbr: RightChildBranch) => cbr.parent = pr
                    }
                  }
                }
                val nq = n.hyperCube.orthant(i)
                val cq = cb.hyperCube
                if (!nq.containsH(cq)) {
                  errors :+= s"Node has invalid hyper-cube ($cq), expected: $nq $assertInfo"
                }
                if (n.hyperCube.indexOfH(cq) != i) {
                  errors :+= s"Mismatch between index-of and used orthant ($i), with parent ${n.hyperCube} and $cq"
                }
                cb.nextOption match {
                  case Some(next) =>
                    if (!next.prevOption.contains(cb)) {
                      errors :+= s"Asymmetric next link $cq $assertInfo"
                    }
                    if (next.hyperCube != cq) {
                      errors :+= s"Next hyper-cube does not match ($cq vs. ${next.hyperCube}) $assertInfo"
                    }
                  case None =>
                    if (nextUnlinkedOcs.contains(cq)) {
                      errors :+= s"Double missing link for $cq $assertInfo"
                    }
                }
                cb.prevOption match {
                  case Some(prev) =>
                    if (!prev.nextOption.contains(cb)) {
                      errors :+= s"Asymmetric prev link $cq $assertInfo"
                    }
                    if (prev.hyperCube != cq) {
                      errors :+= s"Next hyper-cube do not match ($cq vs. ${prev.hyperCube}) $assertInfo"
                    }
                  case None => currUnlinkedOcs += cq
                }
                checkChildren(cb, depth + 1)

              case l: Leaf =>
                currPoints += l // .value

              case _ =>
            }
            i += 1
          }
        }

        checkChildren(h, 0)
        val pointsOnlyInNext = nextPoints.diff(currPoints)
        if (pointsOnlyInNext.nonEmpty) {
          errors :+= s"Points in next which aren't in current (${pointsOnlyInNext.take(10).map(_.value)}); in level $level"
          if (repair && level == 1) {
            assert(h.prevOption.isEmpty)

            def newNode(b: LeftBranch, qIdx: Int, iq: H): LeftChildBranch = {
              val sz  = numOrthants // b.children.length
//              val ch  = tx.newVarArray[LeftChild](sz)
              val ch  = new Array[Var[T, LeftChild]](sz)
              val cid = tx.newId()
              var i = 0
              while (i < sz) {
                ch(i) = cid.newVar[LeftChild](Empty)(tx, LeftChildFormat)
                i += 1
              }
              val parentRef   = cid.newVar[LeftBranch](b    )(tx, LeftBranchFormat)
              val rightRef    = cid.newVar[Next      ](Empty)(tx, RightOptionReader   )
              val n           = new LeftChildBranchImpl(
                octree, cid, parentRef, iq, children = ch, nextRef = rightRef
              )
              b.updateChild(qIdx, n)
              n
            }

            def insert(b: LeftBranch, point: PL, leaf: Leaf): Unit = {
              val qIdx = b.hyperCube.indexOfP(point)
              b.child(qIdx) match {
                case Empty =>
                  b.updateChild(qIdx, leaf)

                case old: LeftNonEmptyChild =>
                  // define the greatest interesting square for the new node to insert
                  // in this node at qIdx:
                  val qn2 = old.union(b.hyperCube.orthant(qIdx), point)
                  // create the new node (this adds it to the children!)
                  val n2 = newNode(b, qIdx, qn2)
                  val oIdx = old.orthantIndexIn(qn2)
                  n2.updateChild(oIdx, old)
                  val lIdx = qn2.indexOfP(point)
                  assert(oIdx != lIdx)
                  // This is a tricky bit! And a reason
                  // why should eventually try to do without
                  // parent pointers at all. Since `old`
                  // may be a leaf whose parent points
                  // to a higher level tree, we need to
                  // check first if the parent is `this`,
                  // and if so, adjust the parent to point
                  // to the new intermediate node `ne`!
                  if (old.parent == this) old.updateParentLeft(n2)
                  n2.updateChild(lIdx, leaf)
              }
            }

            h match {
              case lb: LeftBranch =>
                pointsOnlyInNext.foreach { leaf =>
                  val point = pointView(leaf.value, tx)

                  @tailrec
                  def goDown(b: LeftBranch): Unit = {
                    val idx   = b.hyperCube.indexOfP(point)
                    if (idx < 0) {
                      errors :+= s"Can't repair because $point is not in $lb"
                    } else {
                      b.child(idx) match {
                        case lb1: LeftBranch => goDown(lb1)
                        case _ =>
                          insert(b, point, leaf)
                      }
                    }
                  }

                  goDown(lb)
                }

              case _ =>
                errors +:= "Can't repair because not in left branch !?"
            }
          }
        }
        h = h.prevOption.orNull
        level -= 1
        
        h != null
      }) ()

      errors
    }

    /** Checks the tree for correctness.
     *
     * @param reportOnly if `true` simply scans the tree, if `false` it will apply corrections if necessary
     * @return  empty if no problems were found, otherwise a list of strings describing the problems found
     */
    def verifyConsistency(reportOnly: Boolean)(implicit tx: T): Vec[String] = {
      var errors    = Vec.empty[String]
      var repair    = !reportOnly

      val treeOnlyErrors = verifyOctreeConsistency(reportOnly = reportOnly)
      errors ++= treeOnlyErrors
      if (treeOnlyErrors.nonEmpty) {
        repair    = false // stay on the safe side; require that consistency within tree is repaired first
      }

      // Take skip list as reference. Find if octree levels do not match skip list levels,
      // or whether points in the skip list are not found in the octree.
      skipList.iterator.foreach { leaf =>
        val pv = pointView(leaf.value, tx)

        @tailrec def findLeaf(b: Branch = lastTree,
                              lvl: Int = numLevels, doPrint: Boolean = false): Option[(Branch, Int)] = {
          if (doPrint) errors :+= s"...checking $b in level $lvl"
          val idx = b.hyperCube.indexOfP(pv)
          b.child(idx) match {
            case `leaf` => Some(b -> lvl)

            case cb: Branch if cb.hyperCube.containsP(pv) =>
              findLeaf(cb, lvl = lvl, doPrint = doPrint)

            case _ =>
              b.prevOption match {
                case Some(pb: Branch) =>
                  findLeaf(pb, lvl = lvl - 1, doPrint = doPrint)

                case _ => None
              }
          }
        }

        findLeaf() match {
          case None =>
            val foundLevelSkip = HASkipList.debugFindLevel(skipList, leaf)
            errors :+= s"Severe problem with $leaf - in skip list (level $foundLevelSkip) but octree does not find it"

            if (repair && foundLevelSkip == 1) { // this one is fixable
              try {
                DetSkipOctree.sanitizing = true
                skipList.remove(leaf)
              } finally {
                DetSkipOctree.sanitizing = false
                return errors // hackish!!! skipList iterator possibly invalid, thus abort straight after removal
              }
            }

          case Some((_ /* foundParent */, foundLevel)) =>
            val foundLevelSkip = HASkipList.debugFindLevel(skipList, leaf)
            if (foundLevel != foundLevelSkip) {
              errors :+= s"Severe problem with $leaf - is in skip list level $foundLevelSkip versus octree level $foundLevel"
            }

            val parent  = leaf.parent
            val idx     = parent.hyperCube.indexOfP(pv)
            if (idx < 0) {
              errors :+= s"Severe problem with $leaf - reported parent is $parent which doesn't contain the point $pv"
            } else {
              val saw   = parent.child(idx)
              if (saw != leaf) {
                errors :+= s"$leaf with point $pv reported parent $parent but in orthant $idx we see $saw"

                findLeaf(doPrint = true) match {
                  case Some((b, _)) =>
                    errors :+= s"...that is the correct parent!"
                    if (repair) {
                      leaf.parent = b
                    }

                  case None => errors :+= s"...this is bad. can't locate leaf!"
                }
              }
            }
        }
      }

      // Take octree as reference and see if it contains any points not in the skip list.
      val inSkipList = skipList.toSet

      def checkInTreeLevel(b: Branch, lvl: Int): Unit = {
        val sz = numOrthants
        var i = 0
        while (i < sz) {
          b.child(i) match {
            case l: Leaf if !inSkipList(l) =>
              errors :+= s"Only in octree level $lvl but not skip list: $l"
              if (repair) {
                println(s"\n============== BEFORE REMOVING $l ==============")
                println(debugPrint())
                b.demoteLeaf(pointView(l.value, tx), l)
                println(s"\n============== AFTER REMOVING $l ==============")
                println(debugPrint())
                return  // XXX dirty - but if there is more than one wrong leaf, continuing may reinstall a lonely parent
              }

            case cb: Branch =>
              checkInTreeLevel(cb, lvl)

            case _ =>
          }
          i += 1
        }
      }

      def checkInTree(t: Branch, lvl: Int): Unit = {
        checkInTreeLevel(t, lvl)
        t.prevOption.foreach {
          p => checkInTree(p, lvl - 1)
        }
      }

      checkInTree(lastTree, numLevels)

      errors
    }
  }
}

sealed trait DetSkipOctree[T <: Exec[T], PL, P, H, A]
  extends SkipOctree[T, PL, P, H, A] {

  def verifyConsistency(reportOnly: Boolean)(implicit tx: T): Vec[String]

  def headTree: DetSkipOctree.LeftTopBranch[T, PL, P, H, A]
  def lastTree(implicit tx: T): DetSkipOctree.TopBranch [T, PL, P, H, A]
}