/*
 *  NodeImpl.scala
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
package impl

import de.sciss.model.Change

trait ExprNodeImpl[T <: Txn[T], A]
  extends Expr[T, A]
    with SingleEventNode[T, Change[A]] { self =>
  
  override def toString = s"Expr$id"
}