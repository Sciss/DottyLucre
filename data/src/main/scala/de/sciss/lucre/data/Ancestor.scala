/*
 *  Ancestor.scala
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
package data

import de.sciss.lucre.geom.{DistanceMeasure, IntCube, IntDistanceMeasure3D, IntPoint3D, IntPoint3DLike, IntSpace}
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable, WritableFormat}

object Ancestor {
  private final val SER_VERSION = 65

  private[Ancestor] val cube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)

  private type TreeOrder[T <: Exec[T]] = TotalOrder.Set.Entry[T]

  object Vertex {
    private[Ancestor] def toPoint[T <: Exec[T], Version](v: Vertex[T, Version])(implicit tx: T): IntPoint3D =
      IntPoint3D(v.pre.tag, v.post.tag, v.versionInt)
  }

  sealed trait Vertex[T <: Exec[T], Version] extends Writable with Disposable[T] {

    // ---- abstract ----

    def version: Version

    private[Ancestor] def pre:  TreeOrder[T]
    private[Ancestor] def post: TreeOrder[T]
    private[Ancestor] def tree: Tree[T, Version]

    // ---- implementation ----

    final def isAncestorOf(that: Vertex[T, Version])(implicit tx: T): Boolean =
      versionInt <= that.versionInt &&
        pre .compare(that.pre ) <= 0 &&
        post.compare(that.post) >= 0

    final def versionInt: Int = tree.intView(version)

    final def write(out: DataOutput): Unit = {
      tree.versionFormat.write(version, out)
      pre .write(out)
      post.write(out)
    }

    final def dispose()(implicit t: T): Unit = {
      pre .dispose()
      post.dispose()
    }

    override def toString = s"Vertex($version)"
  }

  implicit def treeFormat[T <: Exec[T], Version](
                                                      implicit versionFormat: TFormat[T, Version],
                                                      intView: Version => Int): TFormat[T, Tree[T, Version]] =
    new TreeFmt[T, Version]

  def newTree[T <: Exec[T], Version](rootVersion: Version)(
    implicit tx: T, versionFormat: TFormat[T, Version],
    intView: Version => Int): Tree[T, Version] = {

    new TreeNew[T, Version](rootVersion, tx)
  }

  def readTree[T <: Exec[T], Version](in: DataInput)
                                     (implicit tx: T, versionFormat: TFormat[T, Version],
                                      intView: Version => Int): Tree[T, Version] =
    new TreeRead[T, Version](in, tx)

  private final class TreeFmt[T <: Exec[T], Version](implicit versionFormat: TFormat[T, Version],
                                                     versionView: Version => Int)
    extends WritableFormat[T, Tree[T, Version]] {

    override def readT(in: DataInput)(implicit tx: T): Tree[T, Version] =
      new TreeRead[T, Version](in, tx)

    override def toString = "Ancestor.treeFormat"
  }

  private sealed trait TreeImpl[T <: Exec[T], Version] extends Tree[T, Version] {
    me =>

    // ---- abstract ----

    protected def order: TotalOrder.Set[T]

    // ---- implementation ----

    override def toString = s"Ancestor.Tree(root=$root)"

    implicit protected object VertexFormat extends WritableFormat[T, K] {
      override def readT(in: DataInput)(implicit tx: T): K = new K {
        def tree: Tree[T, Version] = me

        val version : Version                 = versionFormat.readT(in)
        val pre     : TotalOrder.Set.Entry[T] = order       .readEntry(in)
        val post    : TotalOrder.Set.Entry[T] = order       .readEntry(in)
      }
    }

    final def write(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      order.write(out)
      root.write(out)
    }

    final def dispose()(implicit tx: T): Unit = {
      order.dispose()
      root.dispose()
    }

    final def vertexFormat: TFormat[T, K] = VertexFormat

    final def insertChild(parent: K, newChild: Version)(implicit tx: T): K = new K {
      def tree: Tree[T, Version] = me

      val version : Version                 = newChild
      val pre     : TotalOrder.Set.Entry[T] = parent.pre.append()
      val post    : TotalOrder.Set.Entry[T] = pre       .append()
    }

    final def insertRetroChild(parent: K, newChild: Version)(implicit tx: T): K = new K {
      def tree: Tree[T, Version] = me

      val version : Version                 = newChild
      val pre     : TotalOrder.Set.Entry[T] = parent.pre .append ()
      val post    : TotalOrder.Set.Entry[T] = parent.post.prepend()

      override def toString = s"${super.toString}@r-ch"
    }

    final def insertRetroParent(child: K, newParent: Version)(implicit tx: T): K = {
      require(child != root)
      new K {
        def tree: Tree[T, Version] = me

        val version : Version                 = newParent
        val pre     : TotalOrder.Set.Entry[T] = child.pre .prepend()
        val post    : TotalOrder.Set.Entry[T] = child.post.append ()

        override def toString = s"${super.toString}@r-par"
      }
    }
  }

  private final class TreeNew[T <: Exec[T], Version](rootVersion: Version, tx0: T)(
    implicit val versionFormat: TFormat[T, Version], val intView: Version => Int)
    extends TreeImpl[T, Version] {
    me =>

    protected val order: TotalOrder.Set[T] = TotalOrder.Set.empty(0)(tx0)

    val root: K = new K {
      def tree: Tree[T, Version] = me

      def version : Version                 = rootVersion
      val pre     : TotalOrder.Set.Entry[T] = order.root
      val post    : TotalOrder.Set.Entry[T] = pre.appendMax()(tx0)
    }
  }

  private final class TreeRead[T <: Exec[T], Version](in: DataInput, tx0: T)(
    implicit val versionFormat: TFormat[T, Version], val intView: Version => Int)
    extends TreeImpl[T, Version] {

    {
      val serVer = in.readByte()
      if (serVer != SER_VERSION)
        sys.error(s"Incompatible serialized version (found $serVer, required $SER_VERSION).")
    }

    protected val order: TotalOrder.Set[T] = TotalOrder.Set.read(in)(tx0)
    val root: K = VertexFormat.readT(in)(tx0)
  }

  sealed trait Tree[T <: Exec[T], Version] extends Writable with Disposable[T] {
    protected type K = Vertex[T, Version]

    private[Ancestor] def versionFormat: TFormat[T, Version]
    private[Ancestor] def intView: Version => Int

    def vertexFormat: TFormat[T, K]

    def root: K

    def insertChild      (parent: K, newChild : Version)(implicit tx: T): K
    def insertRetroChild (parent: K, newChild : Version)(implicit tx: T): K
    def insertRetroParent(child : K, newParent: Version)(implicit tx: T): K
  }

  private type MarkOrder[T <: Exec[T], Version, A] = TotalOrder.Map.Entry[T, Mark[T, Version, A]]

  private final val chebyshevMetric = IntDistanceMeasure3D.chebyshevXY
  // left-bottom-front
  // = left in pre-order list, right in post-order list, smaller in version
  private final val metric = chebyshevMetric.orthant(2)

  private final class FilterMetric(pred: Int => Boolean) extends IntDistanceMeasure3D.LongImpl {
    type P = IntPoint3DLike
    type H = IntCube

    override def toString = s"Ancestor.FilterMetric@${pred.hashCode.toHexString}"

    def distance(a: P, b: P): Long = {
      if (b.x <= a.x && b.y >= a.y && pred(b.z)) {
        chebyshevMetric.distance(a, b)
      } else maxValue
    }

    def minDistance(p: P, q: H): Long = {
      val qe = q.extent
      val qem1 = qe - 1

      if (q.cx - qe <= p.x && q.cy + qem1 >= p.y && pred(q.cz - qe)) {
        chebyshevMetric.minDistance(p, q)
      } else maxValue
    }

    def maxDistance(p: P, q: H): Long = {
      val qe = q.extent
      val qem1 = qe - 1

      if (q.cx + qem1 <= p.x && q.cy - qe >= p.y && pred(q.cz + qem1)) {
        chebyshevMetric.maxDistance(p, q)
      } else maxValue
    }
  }

  private sealed trait Mark[T <: Exec[T], Version, /* @spec(ValueSpec) */ A] extends Writable {

    // ---- abstract ----

    def fullVertex: Vertex[T, Version]

    def pre:  MarkOrder[T, Version, A]
    def post: MarkOrder[T, Version, A]

    def value: A

    def map: MapImpl[T, Version, A]

    // ---- implementation ----

    final def toPoint(implicit tx: T): IntPoint3D = IntPoint3D(pre.tag, post.tag, fullVertex.versionInt)

    final def write(out: DataOutput): Unit = {
      fullVertex.write(out)
      pre       .write(out)
      post      .write(out)
      map.valueFormat.write(value, out)
    }

    final def removeAndDispose()(implicit tx: T): Unit = {
      map.skip.remove(this)
      pre .removeAndDispose()
      post.removeAndDispose()
    }

    override def toString = s"Mark(${fullVertex.version} -> $value)"
  }

  def newMap[T <: Exec[T], Version, A](full: Tree[T, Version], rootVertex: Vertex[T, Version], rootValue: A)
                                      (implicit tx: T, valueFormat: TFormat[T, A]): Map[T, Version, A] =
    new MapNew[T, Version, A](full, rootVertex, rootValue, tx, valueFormat)

  def readMap[T <: Exec[T], Version, A](in: DataInput, tx: T, full: Tree[T, Version])
                                       (implicit valueFormat: TFormat[T, A]): Map[T, Version, A] =
    new MapRead[T, Version, A](full, in, valueFormat, tx)

  /*
   * The result of isomorphic search (mapping full tree vertex coordinates to marked tree coordinates).
   *
   * @param pre     the nearest mark in the pre-order traversal
   * @param preCmp  the relation between the query (full) vertex and the found mark vertex.
   *                `-1` indicates that the full vertex lies left of the found mark vertex in the pre-order list,
   *                `0` indicates that both refer to the same version, and `1` indicates that the full vertex lies
   *                right to the mark vertex in the pre-order list
   * @param post    the nearest mark in the post-order traversal
   * @param postCmp the relation between the query (full) vertex and the found mark vertex.
   *                `-1` indicates that the full vertex lies left of the found mark vertex in the post-order list,
   *                `0` indicates that both refer to the same version, and `1` indicates that the full vertex lies
   *                right to the mark vertex in the post-order list
   */
  private final class IsoResult[T <: Exec[T], Version, /* @spec(ValueSpec) */ A](val pre:  Mark[T, Version, A],
                                                                                 val preCmp: Int,
                                                                                 val post: Mark[T, Version, A],
                                                                                 val postCmp: Int) {

    override def toString: String = {
      val preS  = if (preCmp  < 0) "< " else if (preCmp  > 0) "> " else "== "
      val postS = if (postCmp < 0) "< " else if (postCmp > 0) "> " else "== "
      s"Iso(pre $preS$pre,post $postS$post)"
    }
  }

  private sealed trait MapImpl[T <: Exec[T], Version, /* @spec(ValueSpec) */ A]
    extends Map[T, Version, A] with TotalOrder.Map.RelabelObserver[T, Mark[T, Version, A]] {
    me =>

    final type M = Mark[T, Version, A]

    // ---- abstract ----

    protected def preOrder:  TotalOrder.Map[T, M]
    protected def postOrder: TotalOrder.Map[T, M]
    protected def preList:   SkipList  .Set[T, M]
    protected def postList:  SkipList  .Set[T, M]

    private[Ancestor] def skip: SkipOctree[T, IntPoint3DLike, IntCube, M]

    // ---- implementation ----

    override def toString = s"Ancestor.Map(tree=$full)"

    protected final def preOrdering: TOrdering[T, M] = new TOrdering[T, M] {
      def compare(a: M, b: M)(implicit tx: T): Int = a.pre compare b.pre
    }

    protected final def postOrdering: TOrdering[T, M] = new TOrdering[T, M] {
      def compare(a: M, b: M)(implicit tx: T): Int = a.post compare b.post
    }

    protected implicit object markFormat extends WritableFormat[T, M] {
      override def readT(in: DataInput)(implicit tx: T): M = new M {
        def map: MapImpl[T, Version, A] = me

        val fullVertex: Vertex[T, Version]        = full.vertexFormat.readT (in)
        val pre       : MarkOrder[T, Version, A]  = preOrder        .readEntry  (in)
        val post      : MarkOrder[T, Version, A]  = postOrder       .readEntry  (in)
        val value     : A                         = valueFormat .readT      (in)
      }
    }

    final def write(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      // note: we ask for the full tree through the format constructor,
      // thus we omit writing it out ourselves
      preOrder .write(out)
      postOrder.write(out)
      preList  .write(out)
      postList .write(out)
      skip     .write(out)
      // root.write( out )
    }

    final def dispose()(implicit tx: T): Unit = {
      preOrder.dispose()
      postOrder.dispose()
      preList.dispose()
      postList.dispose()
      skip.dispose()
    }

    final def add(entry: (K, A))(implicit tx: T): Boolean = {
      val vertex  = entry._1
      val iso0    = query(vertex)
      val iso     = if (iso0.preCmp != 0) iso0 else {
        // replace existing entry
        // XXX TODO -- we could use
        // the iso's pre and succ pointers,
        // but it's getting nasty, so we'll...
        val old: M = new M {
          def map: MapImpl[T, Version, A] = me
          val fullVertex: K                         = vertex
          val value     : A                         = entry._2
          val pre       : MarkOrder[T, Version, A]  = iso0.pre.pre
          val post      : MarkOrder[T, Version, A]  = iso0.pre.post
        }
        assert(preList .remove(old))
        assert(postList.remove(old))
        assert(skip    .remove(old))
        iso0.pre.removeAndDispose() // iso.pre is a VM!

        // ...so we'll just repeat the search for the sake of simplicity.
        query(vertex)
      }
      val mv: M = new M {
        def map         : MapImpl[T, Version, A]    = me
        val fullVertex  : K                         = vertex
        val value       : A                         = entry._2
        val pre         : MarkOrder[T, Version, A]  = preOrder .insert()
        val post        : MarkOrder[T, Version, A]  = postOrder.insert()
        if (iso.preCmp <= 0) {
          preOrder .placeBefore(iso.pre , this)
        } else {
          preOrder .placeAfter (iso.pre , this)
        }
        if (iso.postCmp <= 0) {
          postOrder.placeBefore(iso.post, this)
        } else {
          postOrder.placeAfter (iso.post, this)
        }
      }
      preList  += mv
      postList += mv
      skip.add(mv)
      //      val errors = skip.asInstanceOf[DeterministicSkipOctree[T, _, _]].verifyConsistency(reportOnly = true)
      //      require(errors.isEmpty, errors.mkString("==== ERRORS FOUND ====" ,"\n", ""))
      //      res
    }

    final def +=(entry: (K, A))(implicit tx: T): this.type = {
      add(entry)
      this
    }

    private[this] def query(vertex: K)(implicit tx: T): IsoResult[T, Version, A] = {
      val cfPre = vertex.pre
      val (cmPreN, cmPreCmp) = preList.isomorphicQuery { (that: M) =>
        cfPre.compare(that.fullVertex.pre)
      }
      if (cmPreCmp == 0) return new IsoResult(cmPreN, 0, cmPreN, 0)

      val cfPost = vertex.post
      val (cmPostN, cmPostCmp) = postList.isomorphicQuery { (that: M) =>
        cfPost.compare(that.fullVertex.post)
      }
      new IsoResult(cmPreN, cmPreCmp, cmPostN, cmPostCmp)
    }

    final def remove(vertex: K)(implicit tx: T): Boolean = {
      val iso = query(vertex)
      iso.preCmp == 0 /* && (iso.postCmp == 0) */ && {
        // assert(iso.postCmp == 0)
        iso.pre.removeAndDispose() // iso.pre is a VM!
        true
      }
    }

    final def -=(vertex: K)(implicit tx: T): this.type = {
      remove(vertex)
      this
    }

    final def get(vertex: K)(implicit tx: T): Option[A] = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        Some(iso.pre.value)
      } else None
    }

    // XXX TODO: DRY
    final def nearest(vertex: K)(implicit tx: T): (K, A) = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        (vertex, iso.pre.value)
      } else {
        val preTag  = iso.pre .pre .tag
        val postTag = iso.post.post.tag
        val x       = if (iso.preCmp  < 0) preTag  - 1 else preTag
        val y       = if (iso.postCmp > 0) postTag + 1 else postTag
        val nn      = skip.nearestNeighbor(IntPoint3D(x, y, vertex.versionInt), metric)
        (nn.fullVertex, nn.value)
      }
    }

    final def nearestOption(vertex: K)(implicit tx: T): Option[(K, A)] = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        Some((vertex, iso.pre.value))
      } else {
        nearestWithMetric(vertex, iso, metric)
      }
    }

    final def nearestWithFilter(vertex: K)(p: Int => Boolean)(implicit tx: T): Option[(K, A)] = {
      val iso = query(vertex)
      nearestWithMetric(vertex, iso, new FilterMetric(p))
    }

    private[this] def nearestWithMetric(vertex: K, iso: IsoResult[T, Version, A],
                                        metric: DistanceMeasure[Long, IntPoint3DLike, IntCube])
                                       (implicit tx: T): Option[(K, A)] = {
      val preTag  = iso.pre .pre .tag
      val postTag = iso.post.post.tag
      val x       = if (iso.preCmp  < 0) preTag  - 1 else preTag
      val y       = if (iso.postCmp > 0) postTag + 1 else postTag
      val nnOpt: Option[M] = skip.nearestNeighborOption[Long](IntPoint3D(x, y, vertex.versionInt), metric)
      nnOpt.map { nn => (nn.fullVertex, nn.value) }
    }

    // ---- RelabelObserver ----
    final def beforeRelabeling(iterator: Iterator[M])(implicit tx: T): Unit =
      iterator.foreach(skip -= _)

    final def afterRelabeling(iterator: Iterator[M])(implicit tx: T): Unit =
      iterator.foreach(skip += _)

    final def debugPrint(implicit tx: T): String = {
      val s = skip.toList.map { m =>
        val v = m.fullVertex
        s"{version = ${v.versionInt}, value = ${m.value}, pre = ${v.pre.tag}, post = ${v.post.tag}}"
      }
      s.mkString("[", ", ", "]")
    }
  }

  private final class MapNew[T <: Exec[T], Version, A](val full: Tree[T, Version],
                                                       rootVertex: Vertex[T, Version],
                                                       rootValue: A, tx0: T,
                                                       val valueFormat: TFormat[T, A])
    extends MapImpl[T, Version, A] {
    me =>

    protected val preOrder: TotalOrder.Map[T, M] =
      TotalOrder.Map.empty[T, M](me, _.pre)(tx0, markFormat)

    protected val postOrder: TotalOrder.Map[T, M] =
      TotalOrder.Map.empty[T, M](me, _.post, rootTag = Int.MaxValue)(tx0, markFormat)

    private[Ancestor] val skip: SkipOctree[T, IntPoint3DLike, IntCube, M] = {
      val pointView = (p: M, tx: T) => p.toPoint(tx)
      SkipOctree.empty[T, IntPoint3DLike, IntCube, M](cube)(tx0, pointView, IntSpace.ThreeDim, markFormat)
    }

    protected val root: M = {
      val res: M = new M {
        def map        : MapImpl[T, Version, A]    = me
        val fullVertex : K                         = rootVertex
        def pre        : MarkOrder[T, Version, A]  = preOrder .root
        def post       : MarkOrder[T, Version, A]  = postOrder.root
        val value      : A                         = rootValue

        override def toString = s"Root($value)"
      }
      implicit val tx: T = tx0
      skip += res
      res
    }

    protected val preList: SkipList.Set[T, M] = {
      implicit val ord: TOrdering[T, M] = preOrdering
      implicit val tx: T = tx0
      val res = SkipList.Set.empty[T, M]
      res.add(root)
      res
    }

    protected val postList: SkipList.Set[T, M] = {
      implicit val ord: TOrdering[T, M] = postOrdering
      implicit val tx: T = tx0
      val res = SkipList.Set.empty[T, M]
      res.add(root)
      res
    }
  }

  private final class MapRead[T <: Exec[T], Version, A](val full: Tree[T, Version], in: DataInput,
                                                        val valueFormat: TFormat[T, A], tx0: T)
    extends MapImpl[T, Version, A] {
    me =>

    {
      val serVer = in.readByte()
      if (serVer != SER_VERSION)
        sys.error(s"Incompatible serialized version (found $serVer, required $SER_VERSION).")
    }

    protected val preOrder: TotalOrder.Map[T, M] =
      TotalOrder.Map.read[T, M](in, me, _.pre)(tx0, markFormat)

    protected val postOrder: TotalOrder.Map[T, M] =
      TotalOrder.Map.read[T, M](in, me, _.post)(tx0, markFormat)

    protected val preList: SkipList.Set[T, M] = {
      implicit val ord: TOrdering[T, M] = preOrdering
      implicit val tx: T = tx0
      SkipList.Set.read[T, M](in)
    }

    protected val postList: SkipList.Set[T, M] = {
      implicit val ord: TOrdering[T, M] = postOrdering
      implicit val tx: T = tx0
      SkipList.Set.read[T, M](in)
    }

    private[Ancestor] val skip: SkipOctree[T, IntPoint3DLike, IntCube, M] = {
      val pointView = (p: M, tx: T) => p.toPoint(tx)
      SkipOctree.read[T, IntPoint3DLike, IntCube, M](in)(tx0, pointView, IntSpace.ThreeDim, markFormat)
    }
  }

  sealed trait Map[T <: Exec[T], Version, A] extends Writable with Disposable[T] {
    type K = Vertex[T, Version]

    def full: Tree[T, Version]

    def debugPrint(implicit tx: T): String

    /**
     * Marks a given key with a given value.
     *
     * @param   entry the key-value pair (where the key is a vertex in the full tree)
     * @return  `true` if the mark is new, `false` if there had been a mark for the given vertex.
     */
    def add(entry: (K, A))(implicit tx: T): Boolean

    def +=(entry: (K, A))(implicit tx: T): this.type

    def remove(vertex: K)(implicit tx: T): Boolean

    def -=(vertex: K)(implicit tx: T): this.type

    /**
     * Queries for a mark at a given version vertex. Unlike `nearest`, this does
     * not search in the map, but merely tests if the given vertex has been
     * marked or not.
     *
     * @param   vertex  the version vertex to look up
     * @return  the value associated with that vertex, or `None` if the vertex is unmarked.
     */
    def get(vertex: K)(implicit tx: T): Option[A]

    /**
     * Finds the nearest marked ancestor of a given version key.
     * Since the map is constructed with a defined root value, this method is
     * guaranteed to succeed&mdash;if there are no other marks in the map,
     * it will return the root value (unless the `version` argument is
     * illegal, i.e. has a version lower than the root vertex' version).
     *
     * @param   vertex  the key to look for. The algorithm searches for
     *                  the nearest ancestor in the marked map with a version less than or
     *                  equal to the given version
     * @return  a pair consisting of the tree vertex found and the value with which
     *          it has been marked. If the query `version` vertex was marked, it will be
     *          that vertex which is returned, and not an ancestor.
     */
    def nearest(vertex: K)(implicit tx: T): (K, A)

    def nearestOption(vertex: K)(implicit tx: T): Option[(K, A)]

    /**
     * Searches for the nearest marked ancestor, where version control is handed over
     * to a custom predicate function. I.e., while `nearestOption` will test for a
     * version that is less than or equal to the query version, the behaviour may be
     * customised here. The predicate function is called with the `versionInt` field
     * of the vertices, e.g. using the tree's `intView`.
     *
     * Only those vertices are considered for which the predicate is `true`.
     *
     * '''Note:''' This currently only works correctly if the predicate tests for
     * version anteriority!
     *
     * @param vertex  the query vertex
     * @param p       the predicate function for the integer view of the vertex versions
     */
    def nearestWithFilter(vertex: K)(p: Int => Boolean)(implicit tx: T): Option[(K, A)]

    def valueFormat: TFormat[T, A]
  }
}