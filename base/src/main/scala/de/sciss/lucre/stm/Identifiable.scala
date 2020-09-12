/*
 *  Identifiable.scala
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

trait Identifiable[T <: Exec[T] /*+Id*/] {
//  def id: Id
  
  def id(implicit tx: T): tx.Id
  
  private[stm] def opaqueId: Ident[T]

  override def equals(that: Any): Boolean = that match {
    case m: Identifiable[_] =>
      opaqueId == m.opaqueId
    case _ => super.equals(that)
  }

  override def hashCode: Int = opaqueId.hashCode()
}