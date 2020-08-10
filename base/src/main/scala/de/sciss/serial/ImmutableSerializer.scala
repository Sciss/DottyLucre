/*
 *  Serializer.scala
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

import de.sciss.serial.impl.{ImmutableEitherSerializer, ImmutableIndexedSeqSerializer, ImmutableListSerializer, ImmutableMapSerializer, ImmutableOptionSerializer, ImmutableSetSerializer, ImmutableTuple2Serializer, ImmutableTuple3Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object ImmutableSerializer {
  // ---- higher-kinded ----

  implicit def option[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Option[A]] =
    new ImmutableOptionSerializer[A](peer)

  implicit def either[A, B](implicit peer1: ImmutableSerializer[A],
                                     peer2: ImmutableSerializer[B]): ImmutableSerializer[Either[A, B]] =
    new ImmutableEitherSerializer[A, B](peer1, peer2)

  implicit def tuple2[A1, A2](implicit peer1: ImmutableSerializer[A1],
                                       peer2: ImmutableSerializer[A2]): ImmutableSerializer[(A1, A2)] =
    new ImmutableTuple2Serializer[A1, A2](peer1, peer2)

  implicit def tuple3[A1, A2, A3](implicit peer1: ImmutableSerializer[A1],
                                           peer2: ImmutableSerializer[A2],
                                           peer3: ImmutableSerializer[A3]): ImmutableSerializer[(A1, A2, A3)] =
    new ImmutableTuple3Serializer[A1, A2, A3](peer1, peer2, peer3)

  implicit def list[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[List[A]] =
    new ImmutableListSerializer[A](peer)

  implicit def set[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Set[A]] =
    new ImmutableSetSerializer[A](peer)

  implicit def indexedSeq[A](implicit peer: ImmutableSerializer[A]): ImmutableSerializer[Vec[A]] =
    new ImmutableIndexedSeqSerializer[A](peer)

  implicit def map[A, B](implicit key  : ImmutableSerializer[A],
                                  value: ImmutableSerializer[B]): ImmutableSerializer[Map[A, B]] =
    new ImmutableMapSerializer[A, B](key, value)
}
trait ImmutableSerializer[A] extends ImmutableReader[A] with Serializer[Any, Any, A]