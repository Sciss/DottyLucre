/*
 *  ConfluentId.scala
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

package de.sciss.lucre.confluent
package impl

import de.sciss.serial.DataOutput

import scala.util.hashing.MurmurHash3

private final class ConfluentId[T <: Txn[T]](val base: Int, val path: Access[T]) extends Identifier[T] {
  override def hashCode: Int = {
    import MurmurHash3._
    val h0  = productSeed
    val h1  = mix(h0, base)
    val h2  = mixLast(h1, path.##)
    finalizeHash(h2, 2)
  }

  def copy(newPath: Access[T]): Identifier[T] = new ConfluentId(base = base, path = newPath)

  override def equals(that: Any): Boolean = that match {
    case b: Identifier[_] => base == b.base && path == b.path
    case _ => false
  }

  def write(out: DataOutput): Unit = {
    out./* PACKED */ writeInt(base)
    path.write(out)
  }

  override def toString: String = path.mkString(s"<$base @ ", ",", ">")

  def dispose(): Unit = ()
}

private final class PartialId[T <: Txn[T]](val base: Int, val path: Access[T]) extends Identifier[T] {
  override def hashCode: Int = {
    import MurmurHash3._
    val h0  = productSeed
    if (path.isEmpty) {
      val h1  = mixLast(h0, base)
      finalizeHash(h1, 1)
    } else {
      val h1  = mix    (h0, base)
      val h2  = mix    (h1, (path.head >> 32).toInt)
      val h3  = mixLast(h2, (path.last >> 32).toInt)
      finalizeHash(h3, 3)
    }
  }

  def copy(newPath: Access[T]): Identifier[T] = new PartialId(base = base, path = newPath)

  override def equals(that: Any): Boolean = that match {
    case b: PartialId[_] =>
      val bp = b.path
      if (path.isEmpty) {
        base == b.base && bp.isEmpty
      } else {
        base == b.base && bp.nonEmpty && path.head == bp.head && path.last == bp.last
      }

    case _ => false
  }

  def write(out: DataOutput): Unit = {
    out./* PACKED */ writeInt(base)
    path.write(out)
  }

  override def toString: String = {
    val tail = if (path.isEmpty) ""
    else {
      val head = path.head
      val tail = path.tail
      val (mid, last) = tail.splitIndex
      mid.mkString(s"${head.toInt}(,", ",", s"),${last.toInt}")
    }
    s"<$base @ $tail>"
  }

  def dispose(): Unit = ()
}
