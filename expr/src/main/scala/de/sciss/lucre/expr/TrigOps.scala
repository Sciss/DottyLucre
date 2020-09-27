/*
 *  TrigOps.scala
 *  (Lucre 4)
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

import de.sciss.lucre.expr.graph.{Act, Ex, TBinaryOp, TTBinaryOp, Trig}

final class TrigOps(private val t: Trig) extends AnyVal {
  def & (that: Trig): Trig = TTBinaryOp(TTBinaryOp.And(), t, that)
  def | (that: Trig): Trig = TTBinaryOp(TTBinaryOp.Or (), t, that)
  def ^ (that: Trig): Trig = TTBinaryOp(TTBinaryOp.Xor(), t, that)

  def filter(that: Ex[Boolean]): Trig = TBinaryOp(TBinaryOp.And(), t, that)

  //  def filterNot (ex: Ex[Boolean]): Trig = {
  //    import ExOps._
  //    filter(!ex)
  //  }

  def ---> (act: Act): act.type = {
    Act.Link(t, act)
    act
  }
}