/*
 *  Lazy.scala
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
package expr
package graph

trait Lazy extends Product {
  type Repr[T <: Txn[T]] <: Disposable[T]

  // this acts as a fast unique reference
  @transient final protected val ref = new AnyRef

  final def expand[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    ctx.visit(ref, mkRepr)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T]
}