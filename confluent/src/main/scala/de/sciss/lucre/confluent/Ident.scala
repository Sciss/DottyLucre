/*
 *  Identifier.scala
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

import de.sciss.lucre.{Ident => LIdent}

trait Ident[T <: Txn[T]] extends LIdent[T] {
  def base: Int  // name, origin, base, agent, ancestry, germ, parent, root
  def path: Access[T]

  def copy(path: Access[T]): Ident[T] // XXX TODO --- should this take a new transaction?
}
