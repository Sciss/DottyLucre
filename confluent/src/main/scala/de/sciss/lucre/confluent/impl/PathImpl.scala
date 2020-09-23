/*
 *  PathImpl.scala
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
package confluent
package impl

import de.sciss.fingertree
import de.sciss.fingertree.{FingerTree, FingerTreeLike}
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

import scala.util.hashing.MurmurHash3

private[confluent] object PathImpl {
  private implicit object PathMeasure extends fingertree.Measure[Long, (Int, Long)] {
    override def toString = "PathMeasure"

    val zero: (Int, Long) = (0, 0L)

    def apply(c: Long): (Int, Long) = (1, c >>> 32)

    def |+|(a: (Int, Long), b: (Int, Long)): (Int, Long) =
      (a._1 + b._1, a._2 + b._2)

    override def |+|(a: (Int, Long), b: (Int, Long), c: (Int, Long)): (Int, Long) =
      (a._1 + b._1 + c._1, a._2 + b._2 + c._2)
  }

  implicit def format[T <: Txn[T], D <: DurableLike.Txn[D]]: TFormat[D, Access[T]] =
    anyFmt.asInstanceOf[Fmt[T, D]]

  private val anyFmt = new Fmt[Confluent.Txn, Durable.Txn]

  private final class Fmt[T <: Txn[T], D <: DurableLike.Txn[D]] extends WritableFormat[D, Access[T]] {
    override def readT(in: DataInput)(implicit tx: D): Access[T] = {
      val sz    = in./* PACKED */ readInt()
      var tree  = FingerTree.empty[(Int, Long), Long](PathMeasure)
      var i = 0
      while (i < sz) {
        tree :+= readPathComponent(in)
        i += 1
      }
      new Path[T](tree)
    }
  }

  @inline private def readPathComponent(in: DataInput): Long = {
    // val hi = in.readInt()
    // val lo = in./* PACKED */ readInt()
    // (hi.toLong << 32) | (lo.toLong & 0xFFFF)
    in.readLong()
  }

  //    def test_empty[T <: Txn[T]]: S#Acc = empty

  private val anyEmpty = new Path[Confluent.Txn](FingerTree.empty(PathMeasure))

  def empty[T <: Txn[T]]: Access[T] = anyEmpty.asInstanceOf[Path[T]]

  def root[T <: Txn[T]]: Access[T] = new Path[T](FingerTree(1L << 32, 1L << 32)(PathMeasure))

  def read[T <: Txn[T]](in: DataInput): Access[T] = {
    val sz    = in./* PACKED */ readInt()
    var tree  = FingerTree.empty[(Int, Long), Long](PathMeasure)
    var i = 0
    while (i < sz) {
      tree :+= readPathComponent(in)
      i += 1
    }
    new Path[T](tree)
  }

  def readAndAppend[T <: Txn[T]](in: DataInput, acc: Access[T])(implicit tx: T): Access[T] = {
    val sz      = in./* PACKED */ readInt()
    val accTree = acc.tree

    val res     = if (accTree.isEmpty) {
      var i = 0
      var tree = FingerTree.empty[(Int, Long), Long](PathMeasure)
      while (i < sz) {
        tree :+= readPathComponent(in)
        i += 1
      }
      tree

    } else if (sz == 0) {
      accTree

    } else {
      var tree    = FingerTree.empty[(Int, Long), Long](PathMeasure)
      val szm = sz - 1
      var i = 0
      while (i < szm) {
        tree :+= readPathComponent(in)
        i += 1
      }
      val lastTerm  = readPathComponent(in)
      val oldLevel  = tx.readTreeVertexLevel(lastTerm)
      val writeTerm = accTree.head
      val newLevel  = tx.readTreeVertexLevel(writeTerm)

      if (oldLevel != newLevel) { // reconstruct a tree split
        tree :+= lastTerm
        tree ++ accTree
      } else {
        tree ++ accTree.tail      // replace terminal version
      }
    }
    new Path[T](res)
  }

  /**
   * The finger tree has elements of type `Long` where the upper 32 bits are the randomized version,
   * and the lower 32 bits are the incremental version. The measure is taking the index and running sum
   * of the tree.
   */
  private final class Path[T <: Txn[T]](val tree: FingerTree[(Int, Long), Long])
    extends Access[T] with FingerTreeLike[(Int, Long), Long, Path[T]] {

    implicit protected def m: fingertree.Measure[Long, (Int, Long)] = PathMeasure

    def !(implicit tx: T): tx.Acc = this

    override def toString: String = mkString("Path(", ", ", ")")

    override def hashCode: Int = {
      import MurmurHash3._
      val m   = sum
      val h0  = productSeed
      val h1  = mix(h0, (m >>> 32).toInt)
      val h2  = mixLast(h1, m.toInt)
      finalizeHash(h2, 2)
    }

    override def equals(that: Any): Boolean = that match {
      case b: PathLike => b.sum == sum
      case _ => false
    }

    def :+(last: Long): Access[T] = wrap(tree :+ last)
    def +:(head: Long): Access[T] = wrap(head +: tree)

    def apply(idx: Int): Long = tree.find1(_._1 > idx)._2

    // XXX TODO testing one two
    def partial: Access[T] = {
      val sz = size
      if (sz == 0) return this

      var res = FingerTree.empty[(Int, Long), Long](PathMeasure)
      if (sz % 2 != 0) {
        println(s"?? partial from index $this")
      }
      res :+= head
      res :+= last
      wrap(res)
    }

    def maxPrefixLength(term: Long): Int = {
      val pre = tree.takeWhile(_._2 < term)
      if (pre.isEmpty || pre.last != term) 0 else pre.measure._1
    }

//    def maxPrefixLength(that: S#Acc): Int = {
//      val ita = tree.iterator
//      val itb = that.tree.iterator
//      var j = 0
//      while (ita.hasNext && itb.hasNext) {
//        val na = ita.next()
//        val nb = itb.next()
//        if (na != nb) return 0
//        j += 1
//      }
//      j
//    }

    def addTerm(term: Long)(implicit tx: T): Access[T] = {
      val t = if (tree.isEmpty) {
        FingerTree.two[(Int, Long), Long](term, term)
      } else {
        val oldLevel = tx.readTreeVertexLevel(this.term)
        val newLevel = tx.readTreeVertexLevel(term)
        if (oldLevel == newLevel) {
          tree.init :+ term
        } else {
          tree :+ term :+ term
        }
      }
      wrap(t)
    }

    def seminal: Access[T] = {
      val e = indexTerm
      val t = term
      wrap(FingerTree.two[(Int, Long), Long](e, t))
    }

    def indexTerm: Long = apply(size - 2)
    def indexSum : Long = sum - (last >>> 32)

    def :-|(suffix: Long): Access[T] = wrap(tree.init :+ suffix)

    def drop(n: Int): Access[T] = {
      val right = tree.dropWhile(_._1 <= n)
      wrap(right)
    }

    def splitIndex: (Access[T], Long) = (init, last)

    def splitAtIndex(idx: Int): (Access[T], Long) = {
      val (pre, t, _) = tree.span1(_._1 <= idx)
      (wrap(pre), t)
    }

    def splitAtSum(hash: Long): (Access[T], Long) = {
      val (pre, t, _) = tree.span1(_._2 <= hash)
      (wrap(pre), t)
    }

    def write(out: DataOutput): Unit = {
      out./* PACKED */ writeInt(size)
      tree.iterator.foreach { l =>
        // out.writeInt((l >>> 32).toInt)
        // out./* PACKED */ writeInt(l.toInt)
        out.writeLong(l)
      }
    }

    def index: Access[T]  = wrap(tree.init)
    def term: Long        = tree.last
    def size: Int         = tree.measure._1
    def sum: Long         = tree.measure._2

    def sumUntil_OLD(n: Int): Long = {
      val res = tree.takeWhile(_._1 <= n).measure._2
//      val test = sumUntil_X(n)
//      if (res != test) {
//        println(s"$this.sumUntil($n) == $res and not $test")
//      }
////      assert(res == sumUntil_X(n))
      res
    }

    def sumUntil(n: Int): Long = if (tree.isEmpty) 0L else tree.find1(_._1 > n)._1._2

    def take(n: Int): Access[T] = {
      val left = tree.takeWhile(_._1 <= n)
      wrap(left)
    }

    def wrap(_tree: FingerTree[(Int, Long), Long]): Path[T] = new Path(_tree)

    def mkString(prefix: String, sep: String, suffix: String): String =
      tree.iterator.map(_.toInt).mkString(prefix, sep, suffix)

//    def mkString(prefix: String, sep: String, suffix: String): String =
//      tree.iterator.map(i => (i >> 32).toInt).mkString(prefix, sep, suffix)

    def info(implicit tx: T): VersionInfo = tx.system.versionInfo(term)

    def takeUntil(timeStamp: Long)(implicit tx: T): Access[T] = tx.system.versionUntil(this, timeStamp)
  }
}