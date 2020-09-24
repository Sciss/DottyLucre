/*
 *  LucreExpr.scala
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

import de.sciss.lucre.expr.graph.Ex

object LucreExpr {
  // type ExprRepr[A] = {type L[~ <: Txn[~]] <: Expr[~, A]}

  //  /** An expression type with installable extensions. */
  //  type TypeExpr1[A, Repr[~ <: Txn[~]] <: expr.Expr[~, A]] = Type.Expr[A, Repr] with Type._1[Repr]

  // The `val` approach is nice because it hides
  // implementation details. Unfortunately that
  // doesn't give us a real companion object,
  // so implicit resolution, for example for
  // serializers, won't work...

  // val IntObj: TypeExpr1[Int, IntObj] = IntImpl

  def init(): Unit = {
    IntObj            .init()
    LongObj           .init()
    DoubleObj         .init()
    BooleanObj        .init()
    StringObj         .init()
    SpanLikeObj       .init()
    SpanObj           .init()
    IntVector         .init()
    DoubleVector      .init()

    ListObj           .init()
    Folder            .init()
    MapObj            .init()
    Artifact          .init()
    ArtifactLocation  .init()

    BiPin             .init()
    BiGroup           .init()

    IntExtensions     .init()
    LongExtensions    .init()
    DoubleExtensions  .init()
    BooleanExtensions .init()
    StringExtensions  .init()
    SpanLikeExtensions.init()
    SpanExtensions    .init()

    Expr.Type         .init()
    Ex                .init()
    graph.Obj         .init()
  }
}