/*
 *  Writer.scala
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

package de.sciss
package serial

import de.sciss.serial.impl.{CollectionWriter, EitherWriter, MapWriter, OptionWriter, Tuple2Writer, Tuple3Writer}

import scala.collection.immutable.{IndexedSeq => Vec}

trait WriterLoPri {
  /** Compatible with `Serializer.option`. */
  implicit def option[A](implicit peer: Writer[A]): Writer[Option[A]] =
    new OptionWriter[A](peer)

  /** Compatible with `Serializer.either`. */
  implicit def either[A, B](implicit peer1: Writer[A],
                                     peer2: Writer[B]): Writer[Either[A, B]] =
    new EitherWriter[A, B](peer1, peer2)

  /** Compatible with `Serializer.tuple2`. */
  implicit def tuple2[A1, A2](implicit peer1: Writer[A1], peer2: Writer[A2]): Writer[(A1, A2) ] =
    new Tuple2Writer[A1, A2]( peer1, peer2 )

  /** Compatible with `Serializer.tuple3`. */
  implicit def tuple3[A1, A2, A3](implicit peer1: Writer[A1],
                                           peer2: Writer[A2],
                                           peer3: Writer[A3]): Writer[(A1, A2, A3)] =
    new Tuple3Writer[A1, A2, A3](peer1, peer2, peer3)

  /** Compatible with `Serializer.list`. */
  implicit def list[A](implicit peer: Writer[A]): Writer[List[A]] =
    new CollectionWriter[A](peer)

  /** Compatible with `Serializer.set`. */
  implicit def set[A](implicit peer: Writer[A]): Writer[Set[A]] =
    new CollectionWriter[A](peer)

  /** Compatible with `Serializer.indexedSeq`. */
  implicit def indexedSeq[A](implicit peer: Writer[A]): Writer[Vec[A]] =
    new CollectionWriter[A](peer)

  /** Compatible with `Serializer.map`. */
  implicit def map[A, B](implicit key: Writer[A], value: Writer[B]): Writer[Map[A, B]] =
    new MapWriter[A, B](key, value)
}

object Writer extends WriterLoPri {
  implicit def serializer[Tx, Acc, A](implicit peer: Serializer[Tx, Acc, A]): Writer[A] = peer
}
trait Writer[-A] {
  def write(v: A, out: DataOutput): Unit
}