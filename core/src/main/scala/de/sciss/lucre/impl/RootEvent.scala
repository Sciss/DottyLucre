/*
 *  Root.scala
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

/** A rooted event does not have sources. This trait provides a simple
 * implementation of `pull` which merely checks if this event has fired or not.
 */
trait RootEvent[T <: Txn[T], +A] extends Event[T, A] {
  private[lucre] final def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A] = Some(pull.resolve[A])
}

/** A generator without further sources. */
trait RootGeneratorEvent[T <: Txn[T], A] extends GeneratorEvent[T, A] with RootEvent[T, A]