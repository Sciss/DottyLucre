/*
 *  CollectionSerializer.scala
 *  (Serial)
 *
 * Copyright (c) 2011-2020 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.serial
package impl

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

abstract class CollectionSerializer[Tx, Acc, A, That <: Traversable[A]] extends Serializer[Tx, Acc, That] {
  def newBuilder: mutable.Builder[A, That]
  def empty     : That

  def peer      : Serializer[Tx, Acc, A]

  final def write(coll: That, out: DataOutput): Unit = {
    out.writeInt(coll.size)
    val ser = peer
    coll.foreach(ser.write(_, out))
  }

  final override def read(in: DataInput, acc: Acc)(implicit tx: Tx): That = {
    val sz = in.readInt()
    if (sz == 0) empty
    else {
      val b = newBuilder
      b.sizeHint(sz)
      val ser = peer
      var rem = sz
      while (rem > 0) {
        b += ser.read(in, acc)
        rem -= 1
      }
      b.result()
    }
  }
}

abstract class ImmutableCollectionSerializer[A, That <: Traversable[A]]
  extends ImmutableSerializer[That] {

  def newBuilder: mutable.Builder[A, That]
  def empty     : That

  def peer      : ImmutableSerializer[A]

  final def write(coll: That, out: DataOutput): Unit = {
    out.writeInt(coll.size)
    val ser = peer
    coll.foreach(ser.write(_, out))
  }

  final override def read(in: DataInput): That = {
    val sz = in.readInt()
    if (sz == 0) empty
    else {
      val b = newBuilder
      b.sizeHint(sz)
      val ser = peer
      var rem = sz
      while (rem > 0) {
        b += ser.read(in)
        rem -= 1
      }
      b.result()
    }
  }
}

final class ListSerializer[Tx, Acc, A](val peer: Serializer[Tx, Acc, A])
  extends CollectionSerializer[Tx, Acc, A, List[A]] {
  def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]
  def empty: List[A] = Nil
}

final class SetSerializer[Tx, Acc, A](val peer: Serializer[Tx, Acc, A])
  extends CollectionSerializer[Tx, Acc, A, Set[A]] {
  def newBuilder: mutable.Builder[A, Set[A]] = Set.newBuilder[A]
  def empty: Set[A] = Set.empty
}

final class IndexedSeqSerializer[Tx, Acc, A](val peer: Serializer[Tx, Acc, A])
  extends CollectionSerializer[Tx, Acc, A, Vec[A]] {
  def newBuilder: mutable.Builder[A, Vec[A]] = Vec.newBuilder[A]
  def empty: Vec[A] = Vector.empty
}

final class ImmutableListSerializer[A](val peer: ImmutableSerializer[A])
  extends ImmutableCollectionSerializer[A, List[A]] {
  def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]
  def empty: List[A] = Nil
}

final class ImmutableSetSerializer[A](val peer: ImmutableSerializer[A])
  extends ImmutableCollectionSerializer[A, Set[A]] {
  def newBuilder: mutable.Builder[A, Set[A]] = Set.newBuilder[A]
  def empty: Set[A] = Set.empty
}

final class ImmutableIndexedSeqSerializer[A](val peer: ImmutableSerializer[A])
  extends ImmutableCollectionSerializer[A, Vec[A]] {
  def newBuilder: mutable.Builder[A, Vec[A]] = Vec.newBuilder[A]
  def empty: Vec[A] = Vector.empty
}