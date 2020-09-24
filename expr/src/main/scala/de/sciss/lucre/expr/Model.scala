/*
 *  Model.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Attr, Ex}

object Model {
  implicit final class Ops[A](private val m: Model[A]) extends AnyVal {
    def <--> (attr: Attr.WithDefault[A]): Unit = {
      this <--- attr
      this ---> attr
    }

    def ---> (attr: Attr.Like[A]): Unit = {
      m.apply() ---> attr
    }

    def ---> (that: Model[A]): Unit =
      that <--- m

    def <--- (value: Ex[A]): Unit =
      m.update(value)

    def <--- (that: Model[A]): Unit =
      m.update(that())
  }

  // XXX TODO: Lucre issue 17
  //  implicit final class OptionOps[A](private val m: Model[Option[A]]) extends AnyVal {
  //    def <--> (attr: Attr[A]): Unit = {
  //      m.update(attr)
  //      m.apply() ---> attr
  //    }
  //  }
}
trait Model[A] {
  def apply(): Ex[A]

  def update(value: Ex[A]): Unit
}
