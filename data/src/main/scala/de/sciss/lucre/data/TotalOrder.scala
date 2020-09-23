/*
 *  TotalOrder.scala
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
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable, WritableFormat}

import scala.annotation.{switch, tailrec}

/**
 * A transactional data structure to maintain an ordered sequence of elements such
 * that two random elements can be compared in O(1).
 *
 * This uses an algorithm from the paper
 * Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J.,
 * Two simplified algorithms for maintaining order in a list,
 * Algorithms—ESA 2002, pp. 219--223, 2002.
 *
 * The `relabel` method is based on the Python implementation by
 * David Eppstein, as published at http://www.ics.uci.edu/~eppstein/PADS/OrderedSequence.py
 * however a bug resulting in a relabel size of 1 was fixed.
 *
 * Original note: "Due to rebalancing on the integer tags used to maintain order,
 * the amortized time per insertion in an n-item list is O(log n)."
 */
object TotalOrder {
  private final val SER_VERSION = 84  // 'T'

  // ---- Set ----

  object Set {
    def empty[T <: Exec[T]](rootTag: Int = 0)(implicit tx: T): Set[T] =
      new SetNew[T](rootTag, tx)

    def read[T <: Exec[T]](in: DataInput)(implicit tx: T): Set[T] =
      new SetRead(in, tx)

    implicit def format[T <: Exec[T]]: TFormat[T, Set[T]] =
      new SetFormat[T]

    sealed trait EntryOption[T <: Exec[T]] {
      protected type E    = Entry[T]
      protected type EOpt = EntryOption[T]

      private[Set] def tagOr      (empty: Int)(implicit t: T): Int
      private[Set] def updatePrev (e: EOpt)   (implicit t: T): Unit
      private[Set] def updateNext (e: EOpt)   (implicit t: T): Unit
      private[Set] def updateTag  (value: Int)(implicit t: T): Unit

      def orNull: E
      def isDefined: Boolean
      def isEmpty: Boolean
    }

    final class EmptyEntry[T <: Exec[T]] private[TotalOrder]() extends EntryOption[T] {
      private[Set] def updatePrev(e: EOpt)(implicit t: T): Unit = ()
      private[Set] def updateNext(e: EOpt)(implicit t: T): Unit = ()

      def orNull: E = null

      private[Set] def updateTag(value: Int)(implicit t: T): Unit =
        sys.error("Internal error - shouldn't be here")

      private[Set] def tagOr(empty: Int)(implicit tx: T) = empty

      def isDefined = false
      def isEmpty   = true

      override def toString = "<empty>"
    }

    final class Entry[T <: Exec[T]] private[TotalOrder](val id: Ident[T],
                                                        set     : Set[T],
                                                        tagVal  : Var[T, Int],
                                                        prevRef : Var[T, EntryOption[T]],
                                                        nextRef : Var[T, EntryOption[T]],
                                                       )
      extends EntryOption[T] with MutableImpl[T] with TOrdered[T, Entry[T]] {

      override def toString = s"Set.Entry$id"

      def compare(that: Entry[T])(implicit tx: T): Int = {
        val thisTag = tag
        val thatTag = that.tag
        if (thisTag < thatTag) -1 else if (thisTag > thatTag) 1 else 0
      }

      def tag                           (implicit t: T): Int  = tagVal()
      private[Set] def tagOr(empty: Int)(implicit t: T)       = tagVal()

      def prev(implicit tx: T): EOpt = prevRef()
      def next(implicit tx: T): EOpt = nextRef()

      private[Set] def prevOrNull(implicit tx: T): E = prevRef().orNull
      private[Set] def nextOrNull(implicit tx: T): E = nextRef().orNull

      def orNull: E = this

      def isDefined = true
      def isEmpty   = false

      private[Set] def updatePrev(e: EOpt)(implicit tx: T): Unit = prevRef() = e
      private[Set] def updateNext(e: EOpt)(implicit tx: T): Unit = nextRef() = e

      private[Set] def updateTag(value: Int)(implicit tx: T): Unit = tagVal() = value

      protected def writeData(out: DataOutput): Unit = {
        tagVal .write(out)
        prevRef.write(out)
        nextRef.write(out)
      }

      protected def disposeData()(implicit tx: T): Unit = {
        prevRef.dispose()
        nextRef.dispose()
        tagVal .dispose()
      }

      def remove()(implicit tx: T): Unit = set.remove(this)

      def append   ()(implicit tx: T): E = set.insertAfter   (this)
      def appendMax()(implicit tx: T): E = set.insertMaxAfter(this)
      def prepend  ()(implicit tx: T): E = set.insertBefore  (this)

      def removeAndDispose()(implicit tx: T): Unit = {
        remove()
        dispose()
      }

      def validate(msg: => String)(implicit tx: T): Unit = {
        val recTag = tag
        if (prev.isDefined) {
          val prevTag = prev.orNull.tag
          assert(prevTag < recTag, s"prev $prevTag >= rec $recTag - $msg")
        }
        if (next.isDefined) {
          val nextTag = next.orNull.tag
          assert(recTag < nextTag, s"rec $recTag >= next $nextTag - $msg")
        }
      }
    }
  }

  private final class SetFormat[T <: Exec[T]] extends WritableFormat[T, Set[T]] {
    override def readT(in: DataInput)(implicit tx: T): Set[T] = new SetRead[T](in, tx)

    override def toString = "Set.format"
  }

  private final class SetRead[T <: Exec[T]](in: DataInput, tx0: T)
    extends Set[T] with MutableImpl[T] {

    val id: Ident[T] = tx0.readId(in)

    {
      val version = in.readByte()
      if (version != SER_VERSION)
        sys.error(s"Incompatible serialized version (found $version, required $SER_VERSION).")
    }

    val sizeVal: Var[T, Int] = id.readIntVar(in)

    val root: Set.Entry[T] = EntryFormat.readT(in)(tx0)
  }

  private final class SetNew[T <: Exec[T]](rootTag: Int, tx0: T)
    extends Set[T] with MutableImpl[T] {
    me =>

    val id: Ident[T] = tx0.newId()

    protected val sizeVal: Var[T, Int] = id.newIntVar(1)(tx0)

    val root: E = {
      val rootId  = tx0.newId()
      val tagVal  = rootId.newIntVar(rootTag)(tx0)
      val prevRef = id.newVar[EOpt](empty)(tx0, EntryOptionFormat)
      val nextRef = id.newVar[EOpt](empty)(tx0, EntryOptionFormat)
      new Set.Entry[T](rootId, me, tagVal, prevRef, nextRef)
    }
  }

  sealed trait Set[T <: Exec[T]] extends TotalOrder[T] {
    me =>
    
    final type           E    = Set.Entry[T]
    protected final type EOpt = Set.EntryOption[T] /* with MutableOption[ S ] */

    protected def sizeVal: Var[T, Int]

    protected final val empty = new Set.EmptyEntry[T]

    // def root: E

    override def toString = s"Set$id"

    final def readEntry(in: DataInput)(implicit tx: T): E =
      EntryFormat.readT(in)

    protected implicit object EntryFormat extends WritableFormat[T, E] {
      override def readT(in: DataInput)(implicit tx: T): E = {
        val id      = tx.readId(in)
        val tagVal  = id.readIntVar(in)
        val prevRef = id.readVar[EOpt](in)(EntryOptionFormat)
        val nextRef = id.readVar[EOpt](in)(EntryOptionFormat)
        new E(id, me, tagVal, prevRef, nextRef)
      }
    }

    protected implicit object EntryOptionFormat extends TFormat[T, EOpt] {
      override def readT(in: DataInput)(implicit tx: T): EOpt = {
        (in.readByte(): @switch) match {
          case 0 => me.empty
          case 1 => EntryFormat.readT(in)
          case cookie => sys.error(s"Unexpected cookie $cookie")
        }
      }

      override def write(v: EOpt, out: DataOutput): Unit = {
        val e = v.orNull
        if (e == null) {
          out.writeByte(0)
        } else {
          out.writeByte(1)
          e.write(out)
        }
      }
    }

    protected final def disposeData()(implicit tx: T): Unit = {
      root.dispose()
      sizeVal.dispose()
    }

    protected final def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      sizeVal.write(out)
      root.write(out)
    }

    private[TotalOrder] final def insertMaxAfter(prev: E)(implicit tx: T): E = {
      val next        = prev.next
      val nextTag     = next.tagOr(Int.MinValue)  // Int.MinValue - 1 == Int.MaxValue !
      val prevTag     = prev.tag
      val n1          = nextTag - 1
      val recTag      = if(prevTag == n1) nextTag else n1
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    private[TotalOrder] final def insertAfter(prev: E)(implicit tx: T): E = {
      val next        = prev.next
      val nextTag     = next.tagOr(Int.MaxValue)
      val prevTag     = prev.tag
      val recTag      = prevTag + ((nextTag - prevTag + 1) >>> 1)
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    private[TotalOrder] final def insertBefore(next: E)(implicit tx: T): E = {
      val prev        = next.prev
      val prevTag     = prev.tagOr(0)
      val nextTag     = next.tag
      val recTag      = prevTag + ((nextTag - prevTag + 1) >>> 1)
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    private def insert(prev: EOpt, next: EOpt, nextTag: Int, recTag: Int)(implicit tx: T): E = {
      val idE         = tx.newId()
      val recPrevRef  = idE.newVar[EOpt](prev)
      val recNextRef  = idE.newVar[EOpt](next)
      val recTagVal   = idE.newIntVar(recTag)
      val rec         = new E(idE, this, recTagVal, prevRef = recPrevRef, nextRef = recNextRef)
      prev.updateNext(rec)
      next.updatePrev(rec)
      sizeVal() = sizeVal() + 1
      if (recTag == nextTag) relabel(rec)
      rec
    }

    private[TotalOrder] final def remove(entry: E)(implicit tx: T): Unit = {
      val p = entry.prev
      val n = entry.next
      p.updateNext(n)
      n.updatePrev(p)
      sizeVal() = sizeVal() - 1
    }

    final def size(implicit tx: T): Int = sizeVal()

    final def head(implicit tx: T): E = {
      var e = root
      var p = e.prevOrNull
      while (p ne null) {
        e = p
        p = p.prevOrNull
      }
      e
    }

    final def tagList(from: E)(implicit tx: T): List[Int] = {
      val b = List.newBuilder[Int]
      var entry = from
      while (entry ne null) {
        b += entry.tag
        entry = entry.nextOrNull
      }
      b.result()
    }

    /** Relabels from a this entry to clean up collisions with
     * its successors' tags.
     *
     * Original remark from Eppstein:
     * "At each iteration of the rebalancing algorithm, we look at
     * a contiguous subsequence of items, defined as the items for which
     * self._tag &~ mask == base.  We keep track of the first and last
     * items in the subsequence, and the number of items, until we find
     * a subsequence with sufficiently low density, at which point
     * we space the tags evenly throughout the available values.
     *
     * The multiplier controls the growth of the threshold density;
     * it is 2/T for the T parameter described by Bender et al.
     * Large multipliers lead to fewer relabels, while small items allow
     * us to handle more items with machine integer tags, so we vary the
     * multiplier dynamically to allow it to be as large as possible
     * without producing integer overflows."
     */
    private def relabel(_first: E)(implicit tx: T): Unit = {
      var mask    = -1
      var thresh  = 1.0
      var num     = 1
      // val mul     = 2/((2*len(self))**(1/30.))
      val mul     = 2 / math.pow((size << 1).toDouble, 1 / 30.0)
      var first   = _first
      var last    = _first
      var base    = _first.tag
      while ({
        var prev = first.prevOrNull
        while ((prev ne null) && (prev.tag & mask) == base) {
          first = prev
          prev  = prev.prevOrNull
          num  += 1
        }
        var next = last.nextOrNull
        while ((next ne null) && (next.tag & mask) == base) {
          last = next
          next = next.nextOrNull
          num += 1
        }
        //         val inc = (mask + 1) / num
        val inc = -mask / num

        // important: we found a corner case where _first is the last
        // element in the list with a value of 0x7FFFFFFF. in this
        // case, if the predecessor is smaller in value, the original
        // algorithm would immediately terminate with num == 1, which
        // will obviously leave the tag unchanged! thus we must add
        // the additional condition that num is greater than 1!
        if (inc >= thresh && num > 1) {
          // found rebalanceable range
          //               observer.beforeRelabeling( first, num )
          //sys.error( "TODO" )

          //            while( !(item eq last) ) {
          // Note: this was probably a bug in Eppstein's code
          // -- it ran for one iteration less which made
          // the test suite fail for very dense tags. it
          // seems now it is correct with the inclusion
          // of last in the tag updating.
          next = first
          var cnt = 0
          while (cnt < num) {
            next.updateTag(base)
            next  = next.nextOrNull
            base += inc
            cnt  += 1
          }
          //sys.error( "TODO" )
          //               observer.afterRelabeling( first, num )
          return
        }
        mask  <<= 1 // next coarse step
        base   &= mask
        thresh *= mul

        mask != 0
      }) ()
      
      sys.error("label overflow")
    }
  }

  // ---- Map ----

  object Map {
    def empty[T <: Exec[T], A](observer: Map.RelabelObserver[T, A], entryView: A => Map.Entry[T, A],
                               rootTag: Int = 0)
                              (implicit tx: T, keyFormat: TFormat[T, A]): Map[T, A] = {
      new MapNew[T, A](observer, entryView, rootTag, tx)
    }

    def read[T <: Exec[T], A](in: DataInput, observer: Map.RelabelObserver[T, A],
                              entryView: A => Map.Entry[T, A])
                             (implicit tx: T, keyFormat: TFormat[T, A]): Map[T, A] = {
      new MapRead[T, A](observer, entryView, in, tx)
    }

    implicit def format[T <: Exec[T], A](observer: Map.RelabelObserver[T, A],
                                             entryView: A => Map.Entry[T, A])
                                            (implicit keyFormat: TFormat[T, A]): TFormat[T, Map[T, A]] =
      new MapFormat[T, A](observer, entryView)

    /**
     * A `RelabelObserver` is notified before and after a relabeling is taking place due to
     * item insertions. The iterator passed to it contains all the items which are relabelled,
     * excluding the one that has caused the relabelling action.
     *
     * Note that there is a tricky case, when an object creates more than one total order entry,
     * and then calls `placeBefore` or `placeAfter` successively on these entries: For the first
     * entry, the iterator will not contain the inserted element, but when the second entry is
     * inserted, the iterator will contain the first entry, with the potential of causing trouble
     * as the entry may be contained in an incompletely initialized object.
     *
     * For example, in the case of storing pre-head, pre-tail and post elements in two orders, make
     * sure that the pre-tail insertion comes last. Because this will happen:
     *
     * (1) pre-head, post and pre-tail entries created (their tags are -1)
     * (2) pre-head placed, may cause relabelling, but then will be excluded from the iterator
     * (3) post placed, may cause relabelling, but then will be excluded from the iterator
     * (4) pre-tail placed, may cause relabelling, and while the pre-tail view will be excluded
     * from the iterator, the previously placed pre-head '''will''' be included in the iterator,
     * showing the item with pre-tail tag of `-1` in `beforeRelabeling`, however, fortunately,
     * with assigned tag in `afterRelabeling`.
     */
    trait RelabelObserver[Tx /* <: Txn[ _ ] */ , -A] {
      /**
       * This method is invoked right before relabelling starts. That is, the items in
       * the `dirty` iterator are about to be relabelled, but at the point of calling
       * this method the tags still carry their previous values.
       */
      def beforeRelabeling(/* inserted: A, */ dirty: Iterator[A])(implicit tx: Tx): Unit

      /**
       * This method is invoked right after relabelling finishes. That is, the items in
       * the `clean` iterator have been relabelled and the tags carry their new values.
       */
      def afterRelabeling(clean: Iterator[A])(implicit tx: Tx): Unit
    }

    final class NoRelabelObserver[Tx, A]
      extends RelabelObserver[Tx, A] {

      def beforeRelabeling(dirty: Iterator[A])(implicit tx: Tx): Unit = ()
      def afterRelabeling (clean: Iterator[A])(implicit tx: Tx): Unit = ()

      override def toString = "NoRelabelObserver"
    }

    final class Entry[T <: Exec[T], A] private[TotalOrder](map: Map[T, A], val id: Ident[T],
                                                           tagVal:  Var[T, Int],
                                                           prevRef: Var[T, KeyOption[T, A]],
                                                           nextRef: Var[T, KeyOption[T, A]])
      extends MutableImpl[T] with TOrdered[T, Entry[T, A]] {

      private type E    = Entry[T, A]       // scalac bug -- this _is_ used
      private type KOpt = KeyOption[T, A]

      def tag(implicit tx: T): Int = tagVal()

      def validate(msg: => String)(implicit tx: T): Unit = {
        val recTag = tag
        if (prev.isDefined) {
          val prevTag = map.entryView(prev.get).tag
          assert(prevTag < recTag, s"prev $prevTag >= rec $recTag - $msg")
        }
        if (next.isDefined) {
          val nextTag = map.entryView(next.get).tag
          assert(recTag < nextTag, s"rec $recTag >= next $nextTag - $msg")
        }
      }

      override def toString = s"Map.Entry$id"

      private[TotalOrder] def prev(implicit tx: T): KOpt = prevRef()
      private[TotalOrder] def next(implicit tx: T): KOpt = nextRef()

      // private[TotalOrder] def prevOrNull( implicit tx: T ) : A = prevRef.get.orNull
      // private[TotalOrder] def nextOrNull( implicit tx: T ) : A = nextRef.get.orNull
      // def orNull : E = this

      private[TotalOrder] def updatePrev(e: KOpt)(implicit tx: T): Unit = prevRef() = e
      private[TotalOrder] def updateNext(e: KOpt)(implicit tx: T): Unit = nextRef() = e

      private[TotalOrder] def updateTag(value: Int)(implicit tx: T): Unit = tagVal() = value

      // ---- Ordered ----

      def compare(that: E)(implicit tx: T): Int = {
        val thisTag = tag
        val thatTag = that.tag
        if (thisTag < thatTag) -1 else if (thisTag > thatTag) 1 else 0
      }

      protected def writeData(out: DataOutput): Unit = {
        tagVal .write(out)
        prevRef.write(out)
        nextRef.write(out)
      }

      protected def disposeData()(implicit tx: T): Unit = {
        prevRef.dispose()
        nextRef.dispose()
        tagVal .dispose()
      }

      def remove()(implicit tx: T): Unit = map.remove(this)

      def removeAndDispose()(implicit tx: T): Unit = {
        remove()
        dispose()
      }
    }
  }

  private[TotalOrder] sealed trait KeyOption[T <: Exec[T], A] extends Writable {
    def orNull: Map.Entry[T, A]

    def isDefined: Boolean
    def isEmpty  : Boolean

    def get: A
  }

  private[TotalOrder] final class EmptyKey[T <: Exec[T], A]
    extends KeyOption[T, A] /* with EmptyMutable */ {

    def isDefined: Boolean = false
    def isEmpty  : Boolean = true

    def get: A = throw new NoSuchElementException("EmptyKey.get")

    def orNull: Map.Entry[T, A] = null

    def write(out: DataOutput): Unit = out.writeByte(0)

    override def toString = "<empty>"
  }

  private[TotalOrder] final class DefinedKey[T <: Exec[T], A](map: Map[T, A], val get: A)
    extends KeyOption[T, A] {

    def isDefined: Boolean = true
    def isEmpty  : Boolean = false

    def orNull: Map.Entry[T, A] = map.entryView(get)

    def write(out: DataOutput): Unit = {
      out.writeByte(1)
      map.keyFormat.write(get, out)
    }

    override def toString: String = get.toString
  }

  private final class MapFormat[T <: Exec[T], A](observer: Map.RelabelObserver[T, A],
                                                     entryView: A => Map.Entry[T, A])
                                                    (implicit keyFormat: TFormat[T, A])
    extends WritableFormat[T, Map[T, A]] {

    override def readT(in: DataInput)(implicit tx: T): Map[T, A] =
      Map.read[T, A](in, observer, entryView) //  new MapRead[T, A](tx, observer, entryView, in)

    override def toString = "Map.format"
  }

  private final class MapRead[T <: Exec[T], A](protected val observer: Map.RelabelObserver[T, A],
                                               val entryView: A => Map.Entry[T, A], in: DataInput, tx0: T)
                                              (implicit private[TotalOrder] val keyFormat: TFormat[T, A])
    extends Map[T, A] with MutableImpl[T] {

    val id: Ident[T] = tx0.readId(in)

    {
      val version = in.readByte()
      require(version == SER_VERSION, s"Incompatible serialized version (found $version, required $SER_VERSION).")
    }

    val sizeVal: Var[T, Int] = id.readIntVar(in)

    val root: Map.Entry[T, A] = EntryFormat.readT(in)(tx0)
  }

  private final class MapNew[T <: Exec[T], A](protected val observer: Map.RelabelObserver[T, A],
                                              val entryView: A => Map.Entry[T, A], rootTag: Int, tx0: T)
                                             (implicit private[TotalOrder] val keyFormat: TFormat[T, A])
    extends Map[T, A] with MutableImpl[T] {

    val id: Ident[T] = tx0.newId()
    
    protected val sizeVal: Var[T, Int] = id.newIntVar(1)(tx0)

    val root: E = {
      implicit val tx: T = tx0
      val idE     = tx.newId()
      val tagVal  = idE.newIntVar(rootTag)
      val prevRef = idE.newVar[KOpt](emptyKey)
      val nextRef = idE.newVar[KOpt](emptyKey)
      new Map.Entry[T, A](this, idE, tagVal, prevRef, nextRef)
    }
  }

  private final class MapEntryFormat[T <: Exec[T], A](map: Map[T, A])
    extends WritableFormat[T, Map.Entry[T, A]] {

    type E    = Map.Entry[T, A]
    type KOpt = KeyOption[T, A]

    override def readT(in: DataInput)(implicit tx: T): E = {
      import map.keyOptionFmt
      val idE     = tx.readId(in)
      val tagVal  = idE.readIntVar(in)
      val prevRef = idE.readVar[KOpt](in)
      val nextRef = idE.readVar[KOpt](in)
      new Map.Entry[T, A](map, idE, tagVal, prevRef, nextRef)
    }
  }

  private final class KeyOptionFormat[T <: Exec[T], A](map: Map[T, A])
    extends WritableFormat[T, KeyOption[T, A]] {

    type KOpt = KeyOption[T, A]

    override def readT(in: DataInput)(implicit tx: T): KOpt = {
      if (in.readByte() == 0) map.emptyKey
      else {
        val key = map.keyFormat.readT(in)
        new DefinedKey(map, key)
      }
    }
  }

  /*
    * A special iterator used for the relabel observer.
    */
  private final class RelabelIterator[T <: Exec[T], A](recOff: Int, num: Int, recE: Map.Entry[T, A],
                                                       firstK: KeyOption[T, A],
                                                       entryView: A => Map.Entry[T, A])(implicit tx: T)
    extends Iterator[A] {

    private var currK: KeyOption[T, A] = firstK
    private var cnt = 0

    def hasNext: Boolean = cnt < num

    def next(): A = {
      if (cnt == num) throw new java.util.NoSuchElementException("next on empty iterator")
      val res = currK.get
      cnt += 1
      // if we're reaching the recE, skip it.
      // that is to say, `num` is the returned iteration sequence,
      // while skipping the newly inserted entry (`recE`).
      // there may be the case that the last returned element
      // (`res`) has a `next` field pointing to `EmptyKey`.
      // This is the reason, why we _must not_ call `get` on the
      // value read from `next`. Instead, we store the key _option_
      // in `currK` and resolve it if there is another valid call
      // to `iterator.next()`.
      val currE = if (cnt == recOff) recE else entryView(res)
      currK = currE.next // .get
      res
    }

    def reset(): Unit = {
      currK = firstK
      cnt = 0
    }
  }

  sealed trait Map[T <: Exec[T], A] extends TotalOrder[T] {
    map =>

    override def toString = s"Map$id"

    final type           E    = Map.Entry[T, A]
    protected final type KOpt = KeyOption[T, A]

    private[TotalOrder] final val emptyKey: KOpt = new EmptyKey[T, A]
    final implicit val EntryFormat: TFormat[T, E] = new MapEntryFormat[T, A](this)
    private[TotalOrder] final implicit val keyOptionFmt: TFormat[T, KOpt] = new KeyOptionFormat[T, A](this)

    protected def sizeVal: Var[T, Int]

    protected def observer: Map.RelabelObserver[T, A]

    private[TotalOrder] def keyFormat: TFormat[T, A]

    def entryView: A => E

    def root: E

    final def readEntry(in: DataInput)(implicit tx: T): E =
      EntryFormat.readT(in)

    protected final def disposeData()(implicit tx: T): Unit = {
      root   .dispose()
      sizeVal.dispose()
    }

    protected final def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      sizeVal.write(out)
      root   .write(out)
    }

    /** Creates a new _unlinked_ entry in the order. The actual insertion (linking)
     * must be done with a successive call to either `placeAfter` or `placeBefore`!
     */
    def insert()(implicit tx: T): E = {
      val idE         = tx.newId()
      val recTagVal   = idE.newIntVar(-1)
      val recPrevRef  = idE.newVar[KOpt](emptyKey)
      val recNextRef  = idE.newVar[KOpt](emptyKey)
      new Map.Entry[T, A](this, idE, recTagVal, recPrevRef, recNextRef)
    }

    def placeAfter(prev: A, key: A)(implicit tx: T): Unit = {
      val prevE = entryView(prev)
      val nextO = prevE.next
      placeBetween(prevE, new DefinedKey[T, A](map, prev), nextO.orNull, nextO, key)
    }

    def placeBefore(next: A, key: A)(implicit tx: T): Unit = {
      val nextE = entryView(next)
      val prevO = nextE.prev
      placeBetween(prevO.orNull, prevO, nextE, new DefinedKey[T, A](map, next), key)
    }

    private[TotalOrder] def placeBetween(prevE: E, prevO: KOpt, nextE: E, nextO: KOpt, key: A)
                                        (implicit tx: T): Unit = {
      val prevTag = if (prevE ne null) prevE.tag else 0 // could use Int.MinValue+1, but that collides with Octree max space
      val nextTag = if (nextE ne null) nextE.tag else Int.MaxValue

      // This assertion does _not_ hold: If we repeatedly prepend to the order,
      // we might end up with a next element having tag 0, which is the same
      // as prev if prev is empty
      //assert( prevTag < nextTag, "placeBetween - prev is " + prevTag + ", while next is " + nextTag )

      val recTag  = prevTag + ((nextTag - prevTag + 1) >>> 1)
      val recE    = entryView(key)

      require(recE.tag == -1 && prevTag >= 0 && nextTag >= 0, {
        val msg = new StringBuilder()
        if (recE.tag != -1) msg.append("Placed key was already placed before. ")
        if (prevTag < 0) msg.append("Predecessor of placed key has not yet been placed. ")
        if (nextTag < 0) msg.append("Successor of placed key has not yet been placed. ")
        msg.toString
      })

      recE.updateTag(recTag)
      recE.updatePrev(prevO)
      recE.updateNext(nextO)
      val defK = new DefinedKey[T, A](this, key)
      if (prevE ne null) prevE.updateNext(defK)
      if (nextE ne null) nextE.updatePrev(defK)
      // sizeVal.transform(_ + 1)
      sizeVal() = sizeVal() + 1
      if (recTag == nextTag) relabel(key, recE)
    }

    private[TotalOrder] def remove(e: E)(implicit tx: T): Unit = {
      val p = e.prev
      val n = e.next
      if (p.isDefined) p.orNull.updateNext(n)
      if (n.isDefined) n.orNull.updatePrev(p)
      // sizeVal.transform(_ - 1)
      sizeVal() = sizeVal() - 1
    }

    final def size(implicit tx: T): Int = sizeVal()

    final def head(implicit tx: T): E = {
      @tailrec def step(e: E): E = {
        val prevO = e.prev
        if (prevO.isEmpty) e else step(prevO.orNull)
      }
      step(root)
    }

    final def tagList(from: E)(implicit tx: T): List[Int] = {
      val b = List.newBuilder[Int]
      @tailrec def step(e: E): List[Int] = {
        b += e.tag
        val nextO = e.next
        if (nextO.isEmpty) b.result()
        else {
          step(nextO.orNull)
        }
      }
      step(from)
    }

    /*
     * Relabels from a this entry to clean up collisions with
     * its successors' tags.
     *
     * Original remark from Eppstein:
     * "At each iteration of the rebalancing algorithm, we look at
     * a contiguous subsequence of items, defined as the items for which
     * self._tag &~ mask == base.  We keep track of the first and last
     * items in the subsequence, and the number of items, until we find
     * a subsequence with sufficiently low density, at which point
     * we space the tags evenly throughout the available values.
     *
     * The multiplier controls the growth of the threshold density;
     * it is 2/T for the T parameter described by Bender et al.
     * Large multipliers lead to fewer relabels, while small items allow
     * us to handle more items with machine integer tags, so we vary the
     * multiplier dynamically to allow it to be as large as possible
     * without producing integer overflows."
     */
    private[this] def relabel(recK: A, recE: E)(implicit tx: T): Unit = {
      var mask    = -1
      var thresh  = 1.0
      var num     = 1
      // val mul     = 2/((2*len(self))**(1/30.))
      val mul     = 2 / math.pow((size << 1).toDouble, 1 / 30.0)
      var firstE  = recE
      var firstK  = recK
      var lastE   = recE
      var base    = recE.tag
      var recOff  = 0

      while ({
        @tailrec def stepLeft(): Unit = {
          val prevO = firstE.prev
          if (prevO.isDefined) {
            val prevK = prevO.get
            val prevE = entryView(prevK)
            if ((prevE.tag & mask) == base) {
              firstE  = prevE
              firstK  = prevK
              num    += 1
              recOff += 1
              stepLeft()
            }
          }
        }
        stepLeft()

        @tailrec def stepRight(): Unit = {
          val nextO = lastE.next
          if (nextO.isDefined) {
            val nextE = entryView(nextO.get)
            if ((nextE.tag & mask) == base) {
              lastE = nextE
              num  += 1
              stepRight()
            }
          }
        }
        stepRight()

        if (num > 1) {
          val inc = -mask / num

          // important: we found a corner case where _first is the last
          // element in the list with a value of 0x7FFFFFFF. in this
          // case, if the predecessor is smaller in value, the original
          // algorithm would immediately terminate with num == 1, which
          // will obviously leave the tag unchanged! thus we must add
          // the additional condition that num is greater than 1!
          if (inc >= thresh) {
            // found rebalanceable range
            val numM1 = num - 1
            val relabelIterator = if (recOff == 0) {
              new RelabelIterator(-1, numM1, recE, firstE.next, entryView)
            } else {
              new RelabelIterator(recOff, numM1, recE, new DefinedKey(this, firstK), entryView)
            }
            observer.beforeRelabeling(/* recK, */ relabelIterator)(tx)

            // Note: this was probably a bug in Eppstein's code
            // -- it ran for one iteration less which made
            // the test suite fail for very dense tags. it
            // seems now it is correct with the inclusion
            // of last in the tag updating.
            var curr = firstE
            var cnt  = 0
            while (cnt < numM1) {
              curr.updateTag(base)
              val nextK = curr.next.get
              base     += inc
              cnt      += 1
              curr      = if (cnt == recOff) recE else entryView(nextK)
            }
            curr.updateTag(base) // last one
            relabelIterator.reset()
            observer.afterRelabeling(/* recK, */ relabelIterator)(tx)
            return
          }
        }
        mask  <<= 1 // next coarse step
        base   &= mask
        thresh *= mul

        mask != 0
      }) ()
      
      sys.error("label overflow")
    }
  }
}
sealed trait TotalOrder[T <: Exec[T]] extends Mutable[T] {
  type E

  /**
   * The initial element created from which you can start to append and prepend.
   */
  def root: E

  /**
   * Returns the head element of the structure. Note that this
   * is O(n) worst case.
   */
  def head(implicit tx: T): E

  /**
   * The number of elements in the order. This is `1` for a newly
   * created order (consisting only of the root element).
   * You will rarely need this information except for debugging
   * purpose. The operation is O(1).
   */
  def size(implicit tx: T): Int

  def tagList(from: E)(implicit tx: T): List[Int]
}
