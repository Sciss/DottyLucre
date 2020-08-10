/*
 *  Var.scala
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

package de.sciss.lucre.stm

import de.sciss.serial.Writable

object Sink {
  def map[Tx, A, B](in: Sink[Tx, A])(fun: B => A): Sink[Tx, B] = new Map(in, fun)

  private final class Map[Tx, A, B](in: Sink[Tx, A], fun: B => A)
    extends Sink[Tx, B] {

    override def toString = s"Sink.map($in)"

    def update(v: B)(implicit tx: Tx): Unit = in() = fun(v)
  }
}

/** A sink is a transactional write access to a value */
trait Sink[-Tx, -A] {
  def update(v: A)(implicit tx: Tx): Unit
}

object Source {
  def map[Tx, A, B](in: Source[Tx, A])(fun: A => B): Source[Tx, B] = new Map(in, fun)

  private final class Map[Tx, A, B](in: Source[Tx, A], fun: A => B)
    extends Source[Tx, B] {

    override def toString = s"Source.map($in)"

    def apply()(implicit tx: Tx): B = fun(in())
  }
}

/** A source is a transactional read access to a value */
trait Source[-Tx, +A] {
  def apply()(implicit tx: Tx): A
}

trait Ref[-Tx, A] extends Sink[Tx, A] with Source[Tx, A]

/** A transactional variable is an identifiable cell allowing the reading and writing of values */
trait Var[-Tx, A] extends Ref[Tx, A] with Writable with Disposable[Tx] {
  def swap(v: A)(implicit tx: Tx): A

  //  def transformAndGet(f: A => A)(implicit tx: Tx): A
  //  def getAndTransform(f: A => A)(implicit tx: Tx): A
}