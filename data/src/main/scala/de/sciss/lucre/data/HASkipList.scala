/*
 *  HASkipList.scala
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

import de.sciss.lucre.impl.MutableImpl
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.{switch, tailrec}
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}
import scala.collection.mutable

/** A transactional version of the deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size).
 * It uses a modified top-down removal algorithm that avoids the need for a second
 * pass as in the original algorithm, and is careful about object creations, so that
 * it will be able to persist the data structure without any unnecessary reads or
 * writes to the store.
 *
 * Three implementation notes: (1) We treat the nodes as immutable at the moment, storing them
 * directly in the S#Val child pointers of their parents. While this currently seems to
 * have a performance advantage (?), we could try to avoid this by using S#Refs for
 * the child pointers, making the nodes become mutable. We could avoid copying the
 * arrays for each insertion or deletion, at the cost of more space, but maybe better
 * performance.
 *
 * (2) The special treatment of `isRight` kind of sucks. Since now that information is
 * also persisted, we might just have two types of branches and leaves, and avoid passing
 * around this flag.
 *
 * (3) Since there is a bug with the top-down one-pass removal, we might end up removing
 * the creation of instances of virtual branches altogether again when replacing the
 * current algorithm by a two-pass one.
 *
 * TODO: nodes or at least leaves should be horizontally connected for a faster iterator
 *       and fast pair (interval) search
 */
object HASkipList {
  private def opNotSupported: Nothing = sys.error("Operation not supported")

  private final val SER_VERSION = 76

  private final class SetSer[T <: Exec[T], A](keyObserver: SkipList.KeyObserver[T, A])
                                             (implicit ordering: Ordering[A],
                                              keySerializer: TSerializer[T, A])
    extends TSerializer[T, HASkipList.Set[T, A]] {

    override def readT(in: DataInput)(implicit tx: T): HASkipList.Set[T, A] =
      HASkipList.Set.read[T, A](in, keyObserver)

    override def write(list: HASkipList.Set[T, A], out: DataOutput): Unit = list.write(out)

    override def toString = "HASkipList.Set.serializer"
  }

  private final class MapSer[T <: Exec[T], A, B](keyObserver: SkipList.KeyObserver[T, A])
                                                (implicit ordering: Ordering[A],
                                                 keySerializer: TSerializer[T, A],
                                                 valueSerializer: TSerializer[T, B])
    extends WritableSerializer[T, HASkipList.Map[T, A, B]] {

    override def readT(in: DataInput)(implicit tx: T): HASkipList.Map[T, A, B] =
      HASkipList.Map.read[T, A, B](in, keyObserver)

    override def toString = "HASkipList.Map.serializer"
  }

  def debugFindLevel[T <: Exec[T], A, E](list: SkipList[T, A, E], key: A): Int =
    list match {
      case impl0: HASkipList[T, A, E] =>
        val h = impl0.height

        @tailrec
        def stepRight(impl: HASkipList[T, A, E], n: Node[T, A, E], lvl: Int): Int = {
          val idx = impl.indexInNodeR(key, n)
          if (idx < 0) lvl
          else if (n.isLeaf) -1
          else {
            val c = n.asBranch.down(idx)
            if (idx < n.size - 1) stepLeft(impl, c, lvl - 1) else stepRight(impl, c, lvl - 1)
          }
        }

        @tailrec
        def stepLeft(impl: HASkipList[T, A, E], n: Node[T, A, E], lvl: Int): Int = {
          val idx = impl.indexInNodeL(key, n)
          if (idx < 0) lvl else if (n.isLeaf) -1 else stepLeft(impl, n.asBranch.down(idx), lvl - 1)
        }

        val c = impl0.top.orNull // topN
        if (c eq null) -1 else stepRight(impl0, c, h)

      case _ => sys.error(s"Not a HA Skip List: $list")
    }

  private final class SetImpl[T <: Exec[T], A](tx: T)(id: Ident[T] /*tx.Id*/, val minGap: Int,
                                               protected val keyObserver: SkipList.KeyObserver[T, A],
                                               _downNode: SetImpl[T, A] => Var[Node[T, A, A]])
                                              (implicit val ordering: Ordering[A],
                                               val keySerializer: TSerializer[T, A])
    extends Impl[T, A, A](tx)(id) with HASkipList.Set[T, A] {

    protected val downNode: Var[Node[T, A, A]] = _downNode(this)

    override def toString = s"SkipList.Set$id"

    def add   (key: A): Boolean = addEntry(key, key).isEmpty
    def remove(key: A): Boolean = removeEntry(key).isDefined

    def firstKey: A = head
    def lastKey : A = last

    def +=(key: A): this.type = {
      addEntry(key, key)
      this
    }

    protected def newLeaf(key: A): Leaf[T, A, A] = {
      val lKeys = Vector[A](key, null.asInstanceOf[A])
      new SetLeaf[T, A](lKeys)
    }

    def writeEntry(key: A, out: DataOutput): Unit = keySerializer.write(key, out)

    override protected def readLeaf(in: DataInput, isRight: Boolean)(implicit tx: T): Leaf[T, A, A] = {
      val sz    = in.readByte().toInt
      val szi   = if (isRight) sz - 1 else sz
      val keys  = Vector.tabulate[A](sz) { i =>
        if (i < szi) keySerializer.readT(in) else null.asInstanceOf[A]
      }
      new SetLeaf[T, A](keys)
    }
  }

  private final class MapImpl[T <: Exec[T], A, B](tx: T)(id: Ident[T] /*tx.Id*/, val minGap: Int,
                                                  protected val keyObserver: SkipList.KeyObserver[T, A],
                                                  _downNode: MapImpl[T, A, B] => Var[Map.Node[T, A, B]])
                                                 (implicit val ordering : Ordering[A],
                                                  val keySerializer     : TSerializer[T, A],
                                                  val valueSerializer   : TSerializer[T, B])
    extends Impl[T, A, (A, B)](tx)(id) with HASkipList.Map[T, A, B] {

    protected val downNode: Var[Map.Node[T, A, B]] = _downNode(this)

    override def toString = s"SkipList.Map$id"

    def put(key: A, value: B): Option[B] = addEntry(key, (key, value)).map(_._2)

    def remove(key: A): Option[B] = removeEntry(key).map(_._2)

    def firstKey: A = head._1
    def lastKey : A = last._1

    def +=(entry: (A, B)): this.type = {
      addEntry(entry._1, entry)
      this
    }

    def writeEntry(entry: (A, B), out: DataOutput): Unit = {
      keySerializer  .write(entry._1, out)
      valueSerializer.write(entry._2, out)
    }

    protected def newLeaf(entry: (A, B)): Leaf[T, A, (A, B)] = {
      val en = Vector(entry, null.asInstanceOf[(A, B)])
      new MapLeaf[T, A, B](en)
    }

    def keysIterator: Iterator[A] = {
      val i = new KeyIteratorImpl
      i.init()
      i
    }

    def valuesIterator: Iterator[B] = {
      val i = new ValueIteratorImpl
      i.init()
      i
    }

    def get(key: A): Option[B] = {
      @tailrec
      def stepRight(n: Node[T, A, (A, B)]): Option[B] = {
        val idx   = indexInNodeR(key, n)
        val idxP  = if (idx < 0) -(idx + 1) else idx
        if (n.isLeaf) {
          if (idx < 0)
            Some(n.asLeaf.entry(idxP)._2)
          else
            None
        } else {
          val c = n.asBranch.down(idxP)
          if (idxP < n.size - 1) stepLeft(c) else stepRight(c)
        }
      }

      @tailrec
      def stepLeft(n: Node[T, A, (A, B)]): Option[B] = {
        val idx  = indexInNodeL(key, n)
        val idxP = if (idx < 0) -(idx + 1) else idx
        if (n.isLeaf) {
          if (idx < 0)
            Some(n.asLeaf.entry(idxP)._2)
          else
            None
        } else {
          stepLeft(n.asBranch.down(idxP))
        }
      }

      val c = topN
      if (c eq null) None else stepRight(c)
    }

    def getOrElse[B1 >: B](key: A, default: => B1): B1 =
      get(key).getOrElse(default) // XXX TODO --- could optimize this at some point

    def getOrElseUpdate(key: A, op: => B): B =
      get(key).getOrElse { // XXX TODO --- could optimize this at some point
        val value = op
        put(key, value)
        value
      }

    private final class KeyIteratorImpl extends IteratorImpl[A] {
      protected def getValue(l: Leaf[T, A, (A, B)], idx: Int): A = l.key(idx)

      override def toString = "KeyIterator"
    }

    private final class ValueIteratorImpl extends IteratorImpl[B] {
      protected def getValue(l: Leaf[T, A, (A, B)], idx: Int): B = l.entry(idx)._2

      override def toString = "ValueIterator"
    }

    override protected def readLeaf(in: DataInput, isRight: Boolean)(implicit tx: T): Leaf[T, A, (A, B)] = {
      val sz  = in.readByte().toInt
      val szi = if (isRight) sz - 1 else sz
      val en  = Vector.tabulate(sz) { i =>
        if (i < szi) {
          val key   = keySerializer  .readT(in)
          val value = valueSerializer.readT(in)
          (key, value)
        } else {
          null.asInstanceOf[(A, B)]
        }
      }
      new MapLeaf[T, A, B](en)
    }
  }

  private abstract class Impl[T <: Exec[T], A, E](tx: T)(final val id: Ident[T]) // (_id: tx.Id)
    extends HASkipList[T, A, E] with HeadOrBranch[T, A, E] with TSerializer[T, Node[T, A, E]] with MutableImpl[T] {
    impl =>

    // ---- abstract ----

    protected def downNode: Var[Node[T, A, E]]
    protected def keyObserver: SkipList.KeyObserver[T, A]

    def writeEntry(entry: E, out: DataOutput): Unit

    protected def newLeaf(entry: E): Leaf[T, A, E]
    protected def readLeaf(in: DataInput, isRight: Boolean)(implicit tx: T): Leaf[T, A, E]

    // ---- impl ----

//    implicit private[this] def head: Impl[T, A, E] = this

    final def         arrMinSz: Int = minGap + 1
    private[this] def arrMaxSz: Int = (minGap + 1) << 1 // aka arrMinSz << 1

    private[this] val hasObserver = keyObserver != SkipList.NoKeyObserver

    protected final def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      out.writeByte(minGap)
      downNode.write(out)
    }

    final def clear(): Unit = {
      // we just replace `downNode`. for confluent,
      // the updates wouldn't make any difference,
      // anyway. for durable, perhaps we have GC some day...

      //      def step(n: Node[T, A, E]): Unit = {
      //        if (n.isBranch) {
      //          val b   = n.asBranch
      //          val bsz = b.size
      //          var i = 0; while (i < bsz) {
      //            step(b.down(i))
      //            b.downRef(i).dispose()
      //            i += 1
      //          }
      //        }
      //      }
      //
      //      val c = topN
      //      if (c ne null) {
      //        step(c)
      downNode() = null
      //      }
    }

    protected override final def disposeData(): Unit =
      downNode.dispose()

    override def size: Int = {
      val c = topN
      if (c eq null) 0 else c.leafSizeSum - 1
    }

    final override def maxGap: Int = (minGap << 1) + 1 // aka arrMaxSz - 1

    final override def isEmpty : Boolean = topN eq null
    final override def nonEmpty: Boolean = !isEmpty

    final override def height: Int = {
      var n = topN
      if (n eq null) 0
      else {
        var h = 1
        while (n.isBranch) {
          n = n.asBranch.down(0)
          h += 1
        }
        h
      }
    }

    final def top: Option[Node[T, A, E]] = Option(topN)

    @inline protected final def topN: Node[T, A, E] = downNode()

    final override def debugPrint(): String = topN.printNode(isRight = true).mkString("\n")

    final override def toIndexedSeq: Vec [E]  = fillBuilder(Vector.newBuilder)
    final override def toList      : List[E]  = fillBuilder(List  .newBuilder)
    final override def toSeq       : Seq [E]  = fillBuilder(Seq   .newBuilder)
    final override def toSet       : ISet[E]  = fillBuilder(ISet  .newBuilder)

    private[this] def fillBuilder[Res](b: mutable.Builder[E, Res]): Res = {
      val i = iterator
      while (i.hasNext) {
        b += i.next() // Txn
      }
      b.result()
    }

    @tailrec
    private[this] def headImpl(n: Node[T, A, E]): E = {
      if (n.isLeaf) {
        n.asLeaf.entry(0)
      } else {
        headImpl(n.asBranch.down(0))
      }
    }

    final override def head: E = {
      val n0 = topN
      if (n0 eq null) throw new NoSuchElementException("head of empty list")
      else headImpl(n0)
    }

    final override def headOption: Option[E] = {
      val n0 = topN
      if (n0 eq null) None
      else Some(headImpl(n0))
    }

    @tailrec
    private[this] def lastImpl(n: Node[T, A, E]): E = {
      if (n.isLeaf) {
        n.asLeaf.entry(n.size - 2)  // N.B. we have `null` as terminator
      } else {
        lastImpl(n.asBranch.down(n.size - 1))
      }
    }

    final override def last: E = {
      val n0 = topN
      if (n0 eq null) throw new NoSuchElementException("last of empty list")
      else lastImpl(n0)
    }

    final override def lastOption: Option[E] = {
      val n0 = topN
      if (n0 eq null) None
      else Some(lastImpl(n0))
    }

    /** Finds the leaf and index in the leaf corresponding to the entry that holds either the given
     * search key or the greatest key in the set smaller than the search key.
     *
     * @param key  the search key
     * @return     if `Some`, holds the leaf and index for the floor element (whose key is <= the search key),
     *             if `None`, there is no key smaller than or equal to the search key in the list
     */
    final override def floor(key: A): Option[E] = {
      // the algorithm is as follows: find the index of the search key in the current level.
      // if the key was found, just go down straight to the leaf. if not:
      //  - the index points to an element greater than the search key
      //  - if the index is >0, let the key at index-1 be the backup-key, the node is the backup-node
      //  - if a leaf is reached and the index is 0
      //       - if no backup-node exists, return None
      //       - if a backup-node exists, follow that node straight down along the backup-key to the leaf
      // In the worst case, we descend the list twice, still giving O(log n) performance, while
      // saving the effort to horizontally connect the leaves.

      @tailrec
      def straight(n: Node[T, A, E], idx: Int): E = {
        if (n.isLeaf) {
          n.asLeaf.entry(idx)
        } else {
          val c = n.asBranch.down(idx)
          straight(c, c.size - 1)
        }
      }

      @tailrec
      def step(n: Node[T, A, E], _bckNode: Node[T, A, E], _bckIdx: Int, isRight: Boolean): Option[E] = {

        val idx = if (isRight) indexInNodeR(key, n) else indexInNodeL(key, n)

        if (idx < 0) {
          // found
          Some(straight(n, -(idx + 1)))
        } else {
          // not found
          var bckNode = _bckNode
          var bckIdx  = _bckIdx
          if (idx > 0) {
            // new backup exists, because there is an entry smaller than the search key
            bckNode = n
            bckIdx  = idx - 1
          }
          if (n.isLeaf) {
            if (bckNode eq null) None else Some(straight(bckNode, bckIdx))
          } else {
            step(n.asBranch.down(idx), bckNode, bckIdx, isRight && idx == n.size - 1)
          }
        }
      }

      val n0 = topN
      if (n0 eq null) None else step(n0, null, 0, isRight = true)
    }

    /** Finds the leaf and index in the leaf corresponding to the entry that holds either the given
     * search key or the smallest key in the set greater than the search key.
     *
     * @param key  the search key
     * @return     if `Some`, holds the leaf and index for the ceiling element (whose key is >= the search key),
     *             if `None`, there is no key greater than or equal to the search key in the list
     */
    final override def ceil(key: A): Option[E] = {
      @tailrec
      def step(n: Node[T, A, E], isRight: Boolean): Option[E] = {
        val idx = if (isRight) indexInNodeR(key, n) else indexInNodeL(key, n)
        val idxP = if (idx < 0) -(idx + 1) else idx
        val newRight = isRight && idxP == n.size - 1
        if (n.isLeaf) {
          if (newRight) None else Some(n.asLeaf.entry(idxP))
        } else {
          step(n.asBranch.down(idxP), newRight)
        }
      }
      val c = topN
      if (c eq null) None else step(c, isRight = true)
    }

    final override def isomorphicQuery(compare: A => Int): (E, Int) = {
      def isoIndexR(n: Node[T, A, E]): Int = {
        var idx = 0
        val sz = n.size - 1
        while ({
          val cmp = compare(n.key(idx))
          if (cmp == 0) return -(idx + 1) else if (cmp < 0) return idx
          idx += 1
          idx < sz
        }) ()
        sz
      }

      def isoIndexL(n: Node[T, A, E]): Int = {
        @tailrec
        def step(idx: Int): Int = {
          val cmp = compare(n.key(idx))
          if (cmp == 0) -(idx + 1) else if (cmp < 0) idx else step(idx + 1)
        }
        step(0)
      }

      @tailrec
      def stepRight(n: Node[T, A, E]): (E, Int) = {
        val idx = isoIndexR(n)
        val found = idx < 0
        val idxP = if (found) -(idx + 1) else idx
        if (n.isLeaf) {
          val l = n.asLeaf
          if (found) {
            (l.entry(idxP), 0)
          } else if (idxP == l.size - 1) {
            (l.entry(idxP - 1), 1)
          } else {
            (l.entry(idxP), -1)
          }
        } else {
          val c = n.asBranch.down(idxP)
          if (idxP < n.size - 1) stepLeft(c) else stepRight(c)
        }
      }

      @tailrec
      def stepLeft(n: Node[T, A, E]): (E, Int) = {
        val idx = isoIndexL(n)
        val found = idx < 0
        val idxP = if (found) -(idx + 1) else idx
        if (n.isLeaf) {
          val l = n.asLeaf
          (l.entry(idxP), if (found) 0 else -1)
        } else {
          stepLeft(n.asBranch.down(idxP))
        }
      }

      val c = topN
      if (c eq null) {
        throw new NoSuchElementException("isomorphicQuery on an empty list")
      } else {
        stepRight(c)
      }
    }

    // ---- set support ----

    final override def contains(v: A): Boolean = {
      @tailrec
      def stepRight(n: Node[T, A, E]): Boolean = {
        val idx = indexInNodeR(v, n)
        if (idx < 0) true
        else if (n.isLeaf) false
        else {
          val c = n.asBranch.down(idx)
          if (idx < n.size - 1) stepLeft(c) else stepRight(c)
        }
      }

      @tailrec
      def stepLeft(n: Node[T, A, E]): Boolean = {
        val idx = indexInNodeL(v, n)
        if (idx < 0) true else if (n.isLeaf) false else stepLeft(n.asBranch.down(idx))
      }

      val c = topN
      if (c eq null) false else stepRight(c)
    }

    final /*protected */ def indexInNodeR(key: A, n: Node[T, A, E]): Int = {
      var idx = 0
      val sz = n.size - 1
      while ({
        val cmp = ordering.compare(key, n.key(idx))
        if (cmp == 0) return -(idx + 1) else if (cmp < 0) return idx
        idx += 1
        idx < sz
      }) ()
      sz
    }

    final /* protected */ def indexInNodeL(key: A, n: Node[T, A, E]): Int = {
      @tailrec
      def step(idx: Int): Int = {
        val cmp = ordering.compare(key, n.key(idx))
        if (cmp == 0) -(idx + 1) else if (cmp < 0) idx else step(idx + 1)
      }
      step(0)
    }

    protected final def addEntry(key: A, entry: E): Option[E] = {
      val c = topN
      if (c eq null) {
        val l = newLeaf(entry)
        downNode() = l
        None
      } else if (c.isLeaf) {
        addToLeaf(key, entry, this /*head*/, 0, this /*head*/, 0, c.asLeaf, isRight = true)
      } else {
        addToBranch(key, entry, this /*head*/, 0, this /*head*/, 0, c.asBranch, isRight = true)
      }
    }

    private[this] def addToLeaf(key: A, entry: E, pp: HeadOrBranch[T, A, E], ppIdx: Int, p: HeadOrBranch[T, A, E],
                                pIdx: Int, l: Leaf[T, A, E], isRight: Boolean): Option[E] = {
      val idx = if (isRight) indexInNodeR(key, l) else indexInNodeL(key, l)
      if (idx < 0) {
        val idxP = -(idx + 1)
        val oldEntry = l.entry(idxP)
        if (entry != oldEntry) {
          val lNew = l.update(idxP, entry)
          p.updateDown(pIdx, lNew)
        }
        Some(oldEntry)

      } else {
        if (l.size == arrMaxSz) {
          val splitKey  = l.key(minGap)
          val tup       = l.splitAndInsert(idx, entry)(this)
          val left      = tup._1
          val right     = tup._2
          val pNew      = p.insertAfterSplit(pIdx, splitKey, left, right, tx)(id /*_id*/)(this)
          pp.updateDown(ppIdx, pNew)
          if (hasObserver) keyObserver.keyUp(splitKey)(tx)
        } else {
          val lNew = l.insert(idx, entry)
          // and overwrite down entry in pn's parent
          p.updateDown(pIdx, lNew)
        }
        None
      }
    }

    @tailrec
    private[this] def addToBranch(key: A, entry: E, pp: HeadOrBranch[T, A, E], ppIdx: Int,
                                  p: HeadOrBranch[T, A, E], pIdx: Int, b: Branch[T, A, E], isRight: Boolean): Option[E] = {
      val idx         = if (isRight) indexInNodeR(key, b) else indexInNodeL(key, b)
      val found       = idx < 0
      val idxP        = if (found) -(idx + 1) else idx
      var bNew        = b
      var idxNew      = idxP
      var pNew        = p
      var pIdxNew     = pIdx
      val bsz         = b.size
      val isRightNew  = isRight && idxP == bsz - 1

      if (!found && bsz == arrMaxSz) {
        val splitKey  = b.key(minGap)
        val tup       = b.split(this)
        val left      = tup._1
        val right     = tup._2
        val pbNew     = p.insertAfterSplit(pIdx, splitKey, left, right, tx)(id /*_id*/)(this)
        pNew          = pbNew
        pp.updateDown(ppIdx, pbNew)
        val mns       = arrMinSz
        if (idx < mns) {
          bNew        = left
        } else {
          bNew        = right
          pIdxNew    += 1
          idxNew     -= mns
        }
        if (hasObserver) keyObserver.keyUp(splitKey)(tx)
      }
      val c = bNew.down(idxNew)
      if (c.isLeaf) {
        addToLeaf  (key, entry, pNew, pIdxNew, bNew, idxNew, c.asLeaf,   isRightNew)
      } else {
        addToBranch(key, entry, pNew, pIdxNew, bNew, idxNew, c.asBranch, isRightNew)
      }
    }

    final override def -=(key: A): this.type = {
      removeEntry(key); this
    }

    protected final def removeEntry(key: A): Option[E] = {
      val c = topN
      if (c eq null) {
        None
      } else if (c.isLeaf) {
        removeFromLeaf  (key, downNode, c.asLeaf  , isRight = true, lDirty = false)
      } else {
        removeFromBranch(key, downNode, c.asBranch, isRight = true, bDirty = false)
      }
    }

    private[this] def removeFromLeaf(key: A, pDown: Sink[Node[T, A, E]], l: Leaf[T, A, E],
                                     isRight: Boolean, lDirty: Boolean): Option[E] = {
      val idx   = if (isRight) indexInNodeR(key, l) else indexInNodeL(key, l)
      val found = idx < 0
      if (found) {
        val idxP = -(idx + 1)
        val lNew = l.removeColumn(idxP)(this)
        pDown()  = if (lNew.size > 1) lNew else null
        Some(l.entry(idxP))
      } else {
        if (lDirty) {
          pDown() = if (l.size > 1) l else null
        }
        None
      }
    }

    @tailrec
    private[this] def removeFromBranchAndBubble(key: A, pDown: Sink[Node[T, A, E]], b: Branch[T, A, E],
                                                leafUpKey: A): Option[E] = {
      val bsz       = b.size
      val idxP      = bsz - 1 // that we know
      val mns       = arrMinSz
      val c         = b.down(idxP)
      val cSz       = c.size

      var bNew      = null: Branch[T, A, E]
      var bDownIdx  = idxP
      var cNew      = c

      if (hasObserver) keyObserver.keyDown(key)(tx)

      // a merge or borrow is necessary either when we descend
      // to a minimally filled child (because that child might
      // need to shrink in the next step)
      if (cSz == mns) {
        // merge with or borrow from the left
        val idxPM1  = idxP - 1
        val cSib    = b.down(idxPM1)
        val cSibSz  = cSib.size

        val downKey = b.key(idxPM1)
        if (hasObserver) keyObserver.keyDown(downKey)(tx)

        if (cSibSz == mns) {
          // merge with the left
          // The parent needs to remove the
          // entry of the left sibling.
          val bNew0   = b.removeColumn(idxPM1)(this)
          bNew        = bNew0.updateKey(idxPM1, leafUpKey) // XXX optimise by merging with previous operation
          b.downRef(idxPM1).dispose()
          bDownIdx    = idxPM1
          cNew        = c.mergeLeft(cSib)
        } else {
          // borrow from the left
          // the parent needs to update the key for the
          // left sibling to match the before-last key in
          // the left sibling.
          val upKey   = cSib.key(cSibSz - 2)
          val bNew0   = b.updateKey(idxPM1, upKey)
          bNew        = bNew0.updateKey(idxP, leafUpKey) // XXX optimise by merging with previous operation
          if (hasObserver) keyObserver.keyUp(upKey)(tx)
          val bDown1  = b.downRef(idxPM1)
          bDown1()    = cSib.removeColumn(cSibSz - 1)(this)
          cNew        = c.borrowLeft(cSib)
        }
      } else {
        bNew = b.updateKey(idxP, leafUpKey)
      }

      if (hasObserver) keyObserver.keyUp(leafUpKey)(tx)

      // branch changed
      val bDown = if (bNew.size > 1) {
        pDown() = bNew // update down ref from which it came
        bNew.downRef(bDownIdx)
      } else {
        // unfortunately we do not have `p`
        //               assert( p == Head )
        bNew.downRef(0).dispose()
        pDown
      }

      if (cNew.isLeaf) {
        removeFromLeaf           (key, bDown, cNew.asLeaf, isRight = false, cNew ne c)
      } else {
        removeFromBranchAndBubble(key, bDown, cNew.asBranch, leafUpKey)
      }
    }

    @tailrec
    private[this] def removeFromBranch(key: A, pDown: Sink[Node[T, A, E]], b: Branch[T, A, E],
                                       isRight: Boolean, bDirty: Boolean): Option[E] = {
      val idx   = if (isRight) indexInNodeR(key, b) else indexInNodeL(key, b)
      val found = idx < 0
      val idxP  = if (found) -(idx + 1) else idx
      val bsz   = b.size
      val mns   = arrMinSz
      val c     = b.down(idxP)
      val cSz   = /* if( cFound ) c.size - 1 else */ c.size

      // if v is found, it will appear in right-most position in all following children.
      // there are two possibilities:
      // (1) a borrow-from-right or merge-with-right is performed. in this case,
      //     v is overwritten in the current branch, thus keep going normally
      //     (no need to specially treat the branch).
      // (2) none of these two operations are performed (because either the child size
      //     is greater than minimum, or v appears in right-most position in b (causing a left op).
      //     -- is this second case possible? no, because we would have encountered
      //     case (1) in the previous iteration, that is, a borrow-from-right or merge-
      //     with-right would have been performed, and thus v cannot appear in right-most
      //     position, and there cannot be a left op.
      // Therefore, we only need to specially treat case (2), that is `cSz > mns`!
      if (found && cSz > mns) {
        // we are here, because the key was found and it would appear in the right-most position
        // in the child, unless we treat it specially here, by finding the key that will bubble
        // up!
        @tailrec def findUpKey(n: Node[T, A, E]): A = {
          if (n.isLeaf) {
            n.key(n.size - 2)
          } else {
            findUpKey(n.asBranch.down(n.size - 1))
          }
        }
        val leafUpKey = findUpKey(c)
        if (hasObserver) keyObserver.keyDown(key)(tx)
        val bNew      = b.updateKey(idxP, leafUpKey)
        if (hasObserver) keyObserver.keyUp(leafUpKey)(tx)

        pDown()       = bNew // update down ref from which we came
        val bDown     = bNew.downRef(idxP)
        return if (c.isLeaf) {
          removeFromLeaf           (key, bDown, c.asLeaf, isRight = false, lDirty = false)
        } else {
          removeFromBranchAndBubble(key, bDown, c.asBranch, leafUpKey)
        }
      }

      var isRightNew  = isRight && idxP == bsz - 1
      var bNew        = b
      var bDownIdx    = idxP
      var cNew        = c

      // a merge or borrow is necessary either when we descend
      // to a minimally filled child (because that child might
      // need to shrink in the next step)
      if (cSz == mns) {
        val idxP1     = idxP + 1
        val bHasRight = idxP1 < bsz
        if (bHasRight) {
          // merge with or borrow from/to the right
          val cSib      = b.down(idxP1)
          val cSibSz    = cSib.size
          val mergedSz  = cSz + cSibSz

          val downKey   = b.key(idxP)
          if (hasObserver) keyObserver.keyDown(downKey)(tx)

          if (mergedSz <= arrMaxSz) {
            // merge with the right
            // remove the entry at idxP from the branch,
            // and actualise b with virtual sibling. the key
            // at bNew's index idxP is now the one formerly at
            // idxP1, hence the right-most key in cSib.
            bNew        = b.removeColumn(idxP)(this)
            b.downRef(idxP).dispose()
            cNew        = c.mergeRight(cSib)
            isRightNew  = isRight && idxP == bsz - 2 // ! we might be in the right-most branch now
          } else {
            // borrow from the right
            assert(cSibSz > mns)
            // update the key index idxP of the
            // originating sibling to match the first key in
            // the right sibling
            val upKey   = cSib.key(0)
            bNew        = b.updateKey(idxP, upKey)
            if (hasObserver) keyObserver.keyUp(upKey)(tx)
            val bDown1  = b.downRef(idxP1)
            bDown1()    = cSib.removeColumn(0)(this)
            cNew        = c.borrowRight(cSib)
          }

        } else {    // merge with or borrow from the left
          // it implies that if cFound is true, cIdx is < c.size - 1
          // that is, the key is not in the last element of c
          // (because otherwise, b would have already in its
          // virtualization be merged to or have borrowed from its right sibling)

          val idxPM1  = idxP - 1
          val cSib    = b.down(idxPM1)
          val cSibSz  = cSib.size

          val downKey = b.key(idxPM1)
          if (hasObserver) keyObserver.keyDown(downKey)(tx)

          if (cSibSz == mns) {    // merge with the left
            // The parent needs to remove the
            // entry of the left sibling.
            bNew      = b.removeColumn(idxPM1)(this)
            b.downRef(idxPM1).dispose()
            bDownIdx  = idxPM1
            cNew      = c.mergeLeft(cSib)
          } else {                // borrow from the left
            // the parent needs to update the key for the
            // left sibling to match the before-last key in
            // the left sibling.
            val upKey   = cSib.key(cSibSz - 2)
            bNew        = b.updateKey(idxPM1, upKey)
            if (hasObserver) keyObserver.keyUp(upKey)(tx)
            val bDown1  = b.downRef(idxPM1)
            bDown1()    = cSib.removeColumn(cSibSz - 1)(this)
            cNew        = c.borrowLeft(cSib)
          }
        }
      }

      val bDown = if (bDirty || (bNew ne b)) {  // branch changed
        if (bNew.size > 1) {
          pDown() = bNew // update down ref from which it came
          bNew.downRef(bDownIdx)
        } else {
          // unfortunately we do not have `p`
          //               assert( p == Head )
          bNew.downRef(0).dispose()
          pDown
        }
      } else {
        bNew.downRef(bDownIdx)
      }

      //         val bDown = bNew.downRef( bDownIdx )
      val cDirty = cNew ne c
      if (cNew.isLeaf) {
        removeFromLeaf  (key, bDown, cNew.asLeaf  , isRight = isRightNew, lDirty = cDirty)
      } else {
        removeFromBranch(key, bDown, cNew.asBranch, isRight = isRightNew, bDirty = cDirty)
      }
    }

    final def iterator: Iterator[E] = {
      val i = new EntryIteratorImpl
      i.init()
      i
    }

    // ---- TSerializer[T, Node[S, A]] ----
    override def write(v: Node[T, A, E], out: DataOutput): Unit =
      if (v eq null) {
        out.writeByte(0) // Bottom
      } else {
        v.write(out)(this)
      }

    override def readT(in: DataInput)(implicit tx: T): Node[T, A, E] = {
      (in.readByte(): @switch) match {
        case 0 => null // .asInstanceOf[ Branch[ S, A ]]
        case 1 => Branch.read(in, isRight = false, id = id)(tx, this)
        case 2 => readLeaf   (in, isRight = false)
        case 5 => Branch.read(in, isRight = true , id = id)(tx, this)
        case 6 => readLeaf   (in, isRight = true )
      }
    }

    private[this] final class EntryIteratorImpl extends IteratorImpl[E] {
      protected def getValue(l: Leaf[T, A, E], idx: Int): E = l.entry(idx)

      override def toString = "Iterator"
    }

    protected sealed abstract class IteratorImpl[C] extends Iterator[C] {
      private[this] var l: Leaf[T, A, E]  = _
      private[this] var nextValue: C      = _
      private[this] var isRight           = true
      private[this] var idx               = 0
      private[this] var stack             = List.empty[(Branch[T, A, E], Int, Boolean)]

      override def toString = s"$impl.iterator"

      protected def getValue(l: Leaf[T, A, E], idx: Int): C

      @tailrec
      private[this] def pushDown(n: Node[T, A, E], idx0: Int, r: Boolean): Unit =
        if (n.isLeaf) {
          val l2    = n.asLeaf
          l         = l2
          idx       = 0
          isRight   = r
          nextValue = getValue(l2, 0) // l2.key( 0 )
        } else {
          val b     = n.asBranch
          stack ::= ((b, idx0 + 1, r))
          pushDown(b.down(idx0), 0, r && idx0 == b.size - 1)
        }

      def init(): Unit = {
        val c = topN
        if (c ne null) pushDown(c, 0, r = true)
      }

      def hasNext: Boolean = l ne null // ordering.nequiv( nextKey, maxKey )

      def next(): C = {
        if (!hasNext) throw new java.util.NoSuchElementException("next on empty iterator")
        val res = nextValue
        idx += 1
        if (idx == (if (isRight) l.size - 1 else l.size) /* || ordering.equiv( l.key( idx ), maxKey ) */ ) {
          @tailrec def popUp(): Unit =
            if (stack.isEmpty) {
              l = null
              nextValue = null.asInstanceOf[C] // maxKey
            } else {
              val (b, i, r) :: tail = stack
              stack = tail
              if (i < b.size) {
                pushDown(b, i, r)
              } else {
                popUp()
              }
            }

          popUp()

        } else {
          nextValue = getValue(l, idx) // l.key( idx )
        }
        res
      }
    }

    def updateDown(i: Int, n: Node[T, A, E]): Unit = {
      if (i != 0) throw new IndexOutOfBoundsException(i.toString)
      downNode() = n
    }

    def insertAfterSplit(pIdx: Int, splitKey: A, left: Node[T, A, E], right: Node[T, A, E], tx: T)
                        (id: Ident[T] /*tx.Id*/)(implicit head: Impl[T, A, E]): Branch[T, A, E] = {
      val bKeys  = Vector[A](splitKey, null.asInstanceOf[A])
      val bDowns = Vector[Var[Node[T, A, E]]](
        id.newVar(left),
        id.newVar(right),
      )
      new Branch[T, A, E](bKeys, bDowns) // new parent branch
    }
  }

  sealed trait HeadOrBranch[T <: Exec[T], A, E] /* extends Branch */ {
    private[HASkipList] def updateDown(i: Int, n: Node[T, A, E]): Unit

    private[HASkipList] def insertAfterSplit(pIdx: Int, splitKey: A, left: Node[T, A, E], right: Node[T, A, E], tx: T)
                                            (id: Ident[T] /*tx.Id*/)(implicit list: Impl[T, A, E]): Branch[T, A, E]
  }

  sealed trait Node[T <: Exec[T], A, E] {

    private[HASkipList] def removeColumn(idx: Int)(implicit /*tx: T,*/ list: Impl[T, A, E]): Node[T, A, E]

    def size: Int

    def key(i: Int): A

    private[HASkipList] def write(out: DataOutput)(implicit list: Impl[T, A, E]): Unit

    private[HASkipList] def leafSizeSum: Int

    private[HASkipList] def printNode(isRight: Boolean): Vec[String]

    /*
     * In merge-with-right, the right sibling's
     * identifier is re-used for the merged node.
     * Thus after the merge, the originating sibling
     * should be disposed (when using an ephemeral
     * data store). The parent needs to remove the
     * entry of the originating sibling.
     *
     * (thus the disposal corresponds with the ref
     * removed from the `downs` array)
     */
    private[HASkipList] def mergeRight(sib: Node[T, A, E]): Node[T, A, E]

    /*
     * In borrow-from-right, both parents' downs need
     * update, but identifiers are kept.
     * the parent needs to update the key for the
     * originating sibling to match the first key in
     * the right sibling (or the new last key in the
     * originating sibling).
     */
    private[HASkipList] def borrowRight(sib: Node[T, A, E]): Node[T, A, E]

    /*
     * In merge-with-left, the originating sibling's
     * identifier is re-used for the merged node.
     * Thus after the merge, the left sibling
     * should be disposed (when using an ephemeral
     * data store). The parent needs to remove the
     * entry of the left sibling.
     *
     * (thus the disposal corresponds with the ref
     * removed from the `downs` array)
     */
    private[HASkipList] def mergeLeft(sib: Node[T, A, E]): Node[T, A, E]

    /*
     * In borrow-from-left, both parents' downs need
     * update, but identifiers are kept.
     * the parent needs to update the key for the
     * left sibling to match the before-last key in
     * the left sibling.
     */
    private[HASkipList] def borrowLeft(sib: Node[T, A, E]): Node[T, A, E]

    def isLeaf:   Boolean
    def isBranch: Boolean

    def asLeaf:   Leaf  [T, A, E]
    def asBranch: Branch[T, A, E]
  }

  private final class SetLeaf[T <: Exec[T], A](private[HASkipList] val entries: Vector[A])
    extends Leaf[T, A, A] {

    protected override def copy(newEntries: Vector[A]): Leaf[T, A, A] = new SetLeaf(newEntries)

    override def key(idx: Int): A = entries(idx)
  }

  private final class MapLeaf[T <: Exec[T], A, B](private[HASkipList] val entries: Vector[(A, B)])
    extends Leaf[T, A, (A, B)] {

    protected override def copy(newEntries: Vector[(A, B)]): Leaf[T, A, (A, B)] = new MapLeaf(newEntries)

    override def key(idx: Int): A = entries(idx)._1
  }

  sealed trait Leaf[T <: Exec[T], A, E] extends Node[T, A, E] {
    override def toString: String = entries.mkString("Leaf(", ",", ")")

    private[HASkipList] def entries: Vector[E]

    final def entry(idx: Int): E = entries(idx)

    protected def copy(newEntries: Vector[E]): Leaf[T, A, E]

    final override def size: Int = entries.size

    final override def isLeaf:   Boolean = true
    final override def isBranch: Boolean = false

    final override def asLeaf:   Leaf[T, A, E] = this
    final override def asBranch: Branch[T, A, E] = opNotSupported

    private[HASkipList] final def leafSizeSum: Int = size

    private[HASkipList] final def printNode(isRight: Boolean): Vec[String] = {
      val sz      = size
      val szm     = sz - 1
      val strings = Seq.tabulate(sz)(idx => if (!isRight || idx < szm) entry(idx).toString else "M")
      Vector(strings.mkString("--"))
    }

    private[HASkipList] final def mergeRight(sib: Node[T, A, E]): Node[T, A, E] = {
      val lSib = sib.asLeaf
      copy(entries ++ lSib.entries)
    }

    private[HASkipList] final def borrowRight(sib: Node[T, A, E]): Node[T, A, E] = {
      val lSib = sib.asLeaf
      copy(entries :+ lSib.entries.head)
    }

    private[HASkipList] final def mergeLeft(sib: Node[T, A, E]): Node[T, A, E] = {
      val lSib = sib.asLeaf
      copy(lSib.entries ++ entries)
    }

    private[HASkipList] final def borrowLeft(sib: Node[T, A, E]): Node[T, A, E] = {
      val lSib = sib.asLeaf
      copy(lSib.entries.last +: entries)
    }

    private[HASkipList] final def insert(idx: Int, entry: E)/*(implicit list: Impl[T, A, E])*/: Leaf[T, A, E] = {
      val newEntries = entries.patch(idx, Vector(entry), 0)
      copy(newEntries)
    }

    private[HASkipList] final def update(idx: Int, entry: E)/*(implicit list: Impl[T, A, E])*/: Leaf[T, A, E] = {
      val newEntries = entries.patch(idx, Vector(entry), 1)
      copy(newEntries)
    }

    private[HASkipList] final def splitAndInsert(idx: Int, entry: E)
                                                (implicit list: Impl[T, A, E]): (Leaf[T, A, E], Leaf[T, A, E]) = {
      //         assert( size == arrMaxSz )
      val arrMinSz = list.arrMinSz
      val (len0, ren0) = entries.splitAt(arrMinSz)
      if (idx < arrMinSz) {
        // split and add `v` to left leaf
        val len   = len0.patch(idx, Vector(entry), 0)
        val left  = copy(len)
        val right = copy(ren0)

        (left, right)

      } else {
        // split and add `v` to right leaf
        val numL  = idx - arrMinSz
        val ren   = ren0.patch(numL, Vector(entry), 0)
        val left  = copy(len0)
        val right = copy(ren)

        (left, right)
      }
    }

    private[HASkipList] final def removeColumn(idx: Int)(implicit /*tx: T,*/ list: Impl[T, A, E]): Leaf[T, A, E] = {
      val newEntries = entries.patch(idx, Vector.empty, 1)
      copy(newEntries)
    }

    private[HASkipList] final def write(out: DataOutput)(implicit list: Impl[T, A, E]): Unit = {
      val sz      = size
      val sz1     = sz - 1
      val isRight = entries(sz1) == null // XXX XXX
      val szi     = if (isRight) sz1 else sz
      out.writeByte(if (isRight) 6 else 2)
      out.writeByte(sz)
      var i = 0
      while (i < szi) {
        list.writeEntry(entries(i), out)
        i += 1
      }
    }
  }

  object Branch {
    private[HASkipList] def read[T <: Exec[T], A, B](in: DataInput, isRight: Boolean, id: Ident[T])
                                                    (implicit tx: T, list: Impl[T, A, B]): Branch[T, A, B] = {
      import list.keySerializer
      val sz    = in.readByte().toInt
      val szi   = if (isRight) sz - 1 else sz
      val keys  = Vector.tabulate(sz) { i =>
        if (i < szi) keySerializer.readT(in) else null.asInstanceOf[A]
      }
      val downs = Vector.fill(sz)(id.readVar[Node[T, A, B]](in))
      new Branch[T, A, B](keys, downs)
    }
  }

  final class Branch[T <: Exec[T], A, B](private[HASkipList] val keys : Vector[A],
                                         private[HASkipList] val downs: Vector[Var[Node[T, A, B]]])
    extends HeadOrBranch[T, A, B] with Node[T, A, B] {

    override def toString: String = keys.mkString("Branch(", ",", ")")

    override def isLeaf:   Boolean = false
    override def isBranch: Boolean = true

    override def asLeaf:   Leaf  [T, A, B] = opNotSupported
    override def asBranch: Branch[T, A, B] = this

    private[HASkipList] def mergeRight(sib: Node[T, A, B]): Node[T, A, B] = {
      val bSib = sib.asBranch
      new Branch[T, A, B](keys ++ bSib.keys, downs ++ bSib.downs)
    }

    private[HASkipList] def borrowRight(sib: Node[T, A, B]): Node[T, A, B] = {
      val bSib = sib.asBranch
      new Branch[T, A, B](keys :+ bSib.keys.head, downs :+ bSib.downs.head)
    }

    private[HASkipList] def mergeLeft(sib: Node[T, A, B]): Node[T, A, B] = {
      val bSib = sib.asBranch
      new Branch[T, A, B](bSib.keys ++ keys, bSib.downs ++ downs)
    }

    private[HASkipList] def borrowLeft(sib: Node[T, A, B]): Node[T, A, B] = {
      val bSib = sib.asBranch
      new Branch[T, A, B](bSib.keys.last +: keys, bSib.downs.last +: downs)
    }

    private[HASkipList] def leafSizeSum: Int = {
      var res = 0
      val sz = size
      var i = 0; while (i < sz) {
        res += down(i).leafSizeSum
        i += 1
      }
      res
    }

    private[HASkipList] def printNode(isRight: Boolean): Vec[String] = {
      val sz   = size
      val szm  = sz - 1
      val columns = Vector.tabulate(sz) { idx =>
        val rr       = isRight && idx == szm
        val child    = down(idx).printNode(rr)
        val childSz  = child.head.length()
        val ks       = if (rr) "M" else key(idx).toString
        val keySz    = ks.length()
        val colSz    = math.max(keySz, childSz) + 2
        val keyAdd   = (if (idx == size - 1) " " else "-") * (colSz - keySz)
        val bar      = s"|${" " * (colSz - 1)}"
        val childAdd = " " * (colSz - childSz)
        Vector(ks + keyAdd, bar) ++ child.map(_ + childAdd)
      }
      Vector.tabulate(columns.map(_.size).max) { row =>
        columns.map(_.apply(row)).mkString("")
      }
    }

    override def key(idx: Int): A = keys(idx)

    override def size: Int = keys.length

    private[HASkipList] def downRef(i: Int): Var[Node[T, A, B]] = downs(i)

    def down(i: Int): Node[T, A, B] = downs(i)()

    private[HASkipList] def split(implicit /*tx: T,*/ list: Impl[T, A, B]): (Branch[T, A, B], Branch[T, A, B]) = {
      val lsz = list.arrMinSz
      val (lKeys , rKeys ) = keys .splitAt(lsz)
      val (lDowns, rDowns) = downs.splitAt(lsz)
      val left  = new Branch[T, A, B](lKeys, lDowns)
      val right = new Branch[T, A, B](rKeys, rDowns)

      (left, right)
    }

    private[HASkipList] def updateDown(i: Int, n: Node[T, A, B]): Unit = downs(i)() = n

    private[HASkipList] def removeColumn(idx: Int)(implicit /*tx: T,*/ list: Impl[T, A, B]): Branch[T, A, B] = {
      val newKeys  = keys .patch(idx, Vector.empty, 1)
      val newDowns = downs.patch(idx, Vector.empty, 1)
      new Branch[T, A, B](newKeys, newDowns)
    }

    private[HASkipList] def updateKey(idx: Int, key: A)/*(implicit tx: T, list: Impl[T, A, B])*/: Branch[T, A, B] = {
      val newKeys = keys.updated(idx, key)
      new Branch[T, A, B](newKeys, downs)
    }

    private[HASkipList] def insertAfterSplit(idx: Int, splitKey: A, left: Node[T, A, B], right: Node[T, A, B], tx: T)
                                            (id: Ident[T] /*tx.Id*/)(implicit list: Impl[T, A, B]): Branch[T, A, B] = {
      // we must make a copy of this branch with the
      // size increased by one. the new key is `splitKey`
      // which gets inserted at the index where we went
      // down, `idx`.
      val bKeys  = keys.patch (idx, Vector(splitKey), 0)
      val bDowns = downs.patch(idx, Vector(id.newVar(left)), 0)

      // copy entries right to split index
      val rightOff       = idx + 1
      bDowns(rightOff)() = right

      new Branch[T, A, B](bKeys, bDowns)
    }

    private[HASkipList] def write(out: DataOutput)(implicit list: Impl[T, A, B]): Unit = {
      import list.keySerializer
      val sz       = size
      val sz1      = sz - 1
      val isRight  = keys(sz1) == null
      val szi      = if (isRight) sz1 else sz
      out.writeByte(if (isRight) 5 else 1)
      out.writeByte(sz)
      var i = 0; while (i < szi) {
        keySerializer.write(keys(i), out)
        i += 1
      }
      i = 0; while (i < sz) {
        downs(i).write(out)
        i += 1
      }
    }
  }

  object Set {
    type Node  [T <: Exec[T], A] = HASkipList.Node  [T, A, A]
    type Branch[T <: Exec[T], A] = HASkipList.Branch[T, A, A]
    type Leaf  [T <: Exec[T], A] = HASkipList.Leaf  [T, A, A]

    /** Creates a new empty skip list with default minimum gap parameter of `2` and no key observer.
     * Type parameter `S` specifies the STM system to use. Type parameter `A`
     * specifies the type of the keys stored in the list.
     *
     * @param   tx          the transaction in which to initialize the structure
     * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
     *                      for specialized versions and transactional restrictions.
     * @param   keySerializer      the serializer for the elements, in case a persistent STM is used.
     */
    def empty[T <: Exec[T], A](implicit tx: T, ord: Ordering[A],
                               keySerializer: TSerializer[T, A]): HASkipList.Set[T, A] =
      empty()

    /** Creates a new empty skip list. Type parameter `S` specifies the STM system to use. Type parameter `A`
     * specifies the type of the keys stored in the list.
     *
     * @param   minGap      the minimum gap-size used for the skip list. This value must be between 1 and 126 inclusive.
     * @param   keyObserver an object which observes key promotions and demotions. Use `NoKeyObserver` (default) if
     *                      key motions do not need to be monitored. The monitoring allows the use of the skip list
     *                      for synchronized decimation of related data structures, such as the deterministic
     *                      skip quadtree.
     * @param   tx          the transaction in which to initialize the structure
     * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
     *                      for specialized versions and transactional restrictions.
     * @param   keySerializer  the serializer for the elements, in case a persistent STM is used.
     */
    def empty[T <: Exec[T], A](minGap: Int = 2,
                               keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                              (implicit tx: T, ord: Ordering[A],
                               keySerializer: TSerializer[T, A]): HASkipList.Set[T, A] = {

      // 255 <= arrMaxSz = (minGap + 1) << 1
      // ; this is, so we can write a node's size as signed byte, and
      // no reasonable app would use a node size > 255
      if (minGap < 1 || minGap > 126) sys.error(s"Minimum gap ($minGap) cannot be less than 1 or greater than 126")

      val implId = tx.newId()
      new SetImpl[T, A](tx)(implId, minGap, keyObserver, list => {
        implId.newVar[Node[T, A]](null)(list)
      })
    }

    def read[T <: Exec[T], A](in: DataInput, keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                             (implicit tx: T, ordering: Ordering[A],
                              keySerializer: TSerializer[T, A]): HASkipList.Set[T, A] = {

      val id      = tx.readId(in)
      val version = in.readByte()
      if (version != SER_VERSION)
        sys.error(s"Incompatible serialized version (found $version, required $SER_VERSION).")

      val minGap = in.readByte().toInt
      new SetImpl[T, A](tx)(id, minGap, keyObserver, list => id.readVar[Node[T, A]](in)(list))
    }

    def serializer[T <: Exec[T], A](keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                   (implicit ordering: Ordering[A],
                                    keySerializer: TSerializer[T, A]): TSerializer[T, HASkipList.Set[T, A]] =
      new SetSer[T, A](keyObserver)
  }

  trait Set[T <: Exec[T], A] extends SkipList.Set[T, A] with HASkipList[T, A, A]

  object Map {
    type Node  [T <: Exec[T], A, B] = HASkipList.Node  [T, A, (A, B)]
    type Branch[T <: Exec[T], A, B] = HASkipList.Branch[T, A, (A, B)]
    type Leaf  [T <: Exec[T], A, B] = HASkipList.Leaf  [T, A, (A, B)]

    /** Creates a new empty skip list with default minimum gap parameter of `2` and no key observer.
     * Type parameter `S` specifies the STM system to use. Type parameter `A`
     * specifies the type of the keys stored in the list.
     *
     * @param   tx          the transaction in which to initialize the structure
     * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
     *                      for specialized versions and transactional restrictions.
     * @param   keySerializer      the serializer for the elements, in case a persistent STM is used.
     */
    def empty[T <: Exec[T], A, B](implicit tx: T, ord: Ordering[A],
                                  keySerializer: TSerializer[T, A],
                                  valueSerializer: TSerializer[T, B]): HASkipList.Map[T, A, B] =
      empty()

    /** Creates a new empty skip list. Type parameter `S` specifies the STM system to use. Type parameter `A`
     * specifies the type of the keys stored in the list.
     *
     * @param   minGap      the minimum gap-size used for the skip list. This value must be between 1 and 126 inclusive.
     * @param   keyObserver an object which observes key promotions and demotions. Use `NoKeyObserver` (default) if
     *                      key motions do not need to be monitored. The monitoring allows the use of the skip list
     *                      for synchronized decimation of related data structures, such as the deterministic
     *                      skip quadtree.
     * @param   tx          the transaction in which to initialize the structure
     * @param   ord         the ordering of the keys. This is an instance of `txn.Ordering` to allow
     *                      for specialized versions and transactional restrictions.
     * @param   keySerializer  the serializer for the elements, in case a persistent STM is used.
     */
    def empty[T <: Exec[T], A, B](minGap: Int = 2,
                                  keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                 (implicit tx: T, ord: Ordering[A],
                                  keySerializer: TSerializer[T, A],
                                  valueSerializer: TSerializer[T, B]): HASkipList.Map[T, A, B] = {

      // 255 <= arrMaxSz = (minGap + 1) << 1
      // ; this is, so we can write a node's size as signed byte, and
      // no reasonable app would use a node size > 255
      if (minGap < 1 || minGap > 126) sys.error(s"Minimum gap ($minGap) cannot be less than 1 or greater than 126")

      val implId = tx.newId()
      new MapImpl[T, A, B](tx)(implId, minGap, keyObserver, list => {
        implId.newVar[Node[T, A, B]](null)(list)
      })
    }

    def read[T <: Exec[T], A, B](in: DataInput, keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                (implicit tx: T, ordering: Ordering[A],
                                 keySerializer: TSerializer[T, A],
                                 valueSerializer: TSerializer[T, B]): HASkipList.Map[T, A, B] = {

      val id      = tx.readId(in)
      val version = in.readByte()
      if (version != SER_VERSION) sys.error(s"Incompatible serialized version (found $version, required $SER_VERSION).")

      val minGap = in.readByte().toInt
      new MapImpl[T, A, B](tx)(id, minGap, keyObserver, list => id.readVar[Node[T, A, B]](in)(list))
    }

    def serializer[T <: Exec[T], A, B](keyObserver: SkipList.KeyObserver[T, A] = SkipList.NoKeyObserver)
                                      (implicit ordering: Ordering[A],
                                       keySerializer: TSerializer[T, A],
                                       valueSerializer: TSerializer[T, B]): TSerializer[T, HASkipList.Map[T, A, B]] =
      new MapSer[T, A, B](keyObserver)
  }

  trait Map[T <: Exec[T], A, B]
    extends SkipList.Map[T, A, B] with HASkipList[T, A, (A, B)]
}
trait HASkipList[T <: Exec[T], A, E] extends SkipList[T, A, E] {
  def top: Option[HASkipList.Node[T, A, E]]

  /** Finds the right-most key which
   * is greater than or equal to the query key.
   *
   * @param   key  the key to search for
   * @param   n the branch or leaf from which to go down
   *
   * @return  the index to go down (a node whose key is greater than `key`),
   *         or `-(index+1)` if `key` was found at `index`
   */
  def indexInNodeR(key: A, n: HASkipList.Node[T, A, E]): Int

  def indexInNodeL(key: A, n: HASkipList.Node[T, A, E]): Int
}