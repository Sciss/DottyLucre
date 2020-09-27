/*
 *  Ref.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre

trait Source[T <: Txn[T], +A] extends lucre.Source[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}

trait Ref[T <: Txn[T], A] extends lucre.Ref[T, A] with Source[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}
