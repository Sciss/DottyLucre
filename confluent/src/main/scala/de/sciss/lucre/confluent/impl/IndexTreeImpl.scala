/*
 *  IndexTreeImpl.scala
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

package de.sciss.lucre.confluent.impl

import de.sciss.lucre.data.Ancestor
import de.sciss.lucre.{DurableLike, Disposable}
import de.sciss.serial.{DataOutput, Writable}

private[impl] trait IndexTree[D <: DurableLike.Txn[D]] extends Writable with Disposable[D] {
  def tree : Ancestor.Tree[D, Long]
  def level: Int
  def term : Long
}

// an index tree holds the pre- and post-lists for each version (full) tree
private[impl] final class IndexTreeImpl[D <: DurableLike.Txn[D]](val tree: Ancestor.Tree[D, Long], val level: Int)
  extends IndexTree[D] {

  override def hashCode: Int = term.toInt

  def term: Long = tree.root.version

  override def equals(that: Any): Boolean = that match {
    case b: IndexTree[_] => term == b.term
    case _ => false
  }

  def write(out: DataOutput): Unit = {
    tree.write(out)
    out./* PACKED */ writeInt(level)
  }

  def dispose()(implicit tx: D): Unit = tree.dispose()

  override def toString = s"IndexTree<v=${term.toInt}, l=$level>"
}