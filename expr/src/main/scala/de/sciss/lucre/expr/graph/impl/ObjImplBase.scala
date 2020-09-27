/*
 *  ObjImplBase.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.{Sys, Txn, Obj => LObj, Source => LSource}

abstract class ObjImplBase[In <: Txn[In], Repr[~ <: Txn[~]] <: LObj[~]](in: LSource[In, Repr[In]], system: Sys)
  extends Obj {

  type Peer[~ <: Txn[~]] = Repr[~]

  private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Repr[T]] = {
    require (tx.system == system)
    val out = in.asInstanceOf[LSource[T, Repr[T]]]
    Some(out())
  }
}