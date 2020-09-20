/*
 *  Source.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.TSource

trait Source[T <: Txn[T], +A] extends TSource[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}
