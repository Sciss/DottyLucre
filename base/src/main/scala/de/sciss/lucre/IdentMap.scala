/*
 *  IdentifierMap.scala
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

/** An identifier map is basically a transactional map whose keys are system identifiers.
 * However, there are two important aspects: First, the map is always ephemeral
 * (but might be still durable!), even for a confluently persistent system. Second,
 * for systems whose identifiers constitute temporal traces (confluently persistent
 * system), lookup (via `get`, `contains` etc.) finds _any_ value stored for the
 * current version or any older version. That is to say, in a confluently persistent
 * system, it looks up the most recent entry for the key. It is therefore a useful
 * tool to map system entities to ephemeral live views.
 *
 * @tparam T    the underlying system's transaction type
 * @tparam A    the values stored at the keys. `Unit` can be used if only set
 *              functionality is needed.
 */
trait IdentMap[T <: Exec[T], A] extends Disposable[T] {
  def put      (id: Ident[T], value: A)     (implicit tx: T): Unit
  def get      (id: Ident[T])               (implicit tx: T): Option[A]
  def getOrElse(id: Ident[T], default: => A)(implicit tx: T): A

  def contains (id: Ident[T])(implicit tx: T): Boolean
  def remove   (id: Ident[T])(implicit tx: T): Unit
}
