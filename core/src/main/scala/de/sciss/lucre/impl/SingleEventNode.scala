/*
 *  SingleEventNode.scala
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

package de.sciss.lucre
package impl

/** Standalone events unite a node and one particular event.
 *
 * WARNING: the implementations of `equals` are really tricky right now. `EventImpl` is more specific in that
 * `VirtualNodeSelector` checks if the compared object is another `VirtualNodeSelector` whose reactor has the
 * same id and whose slot is the same. On the other hand `Invariant` inherits `equals` from `Reactor`
 * which checks for another reactor and then compares their ids.
 *
 * I don't know if `Reactor` still needs the `equals` implementation?
 */
trait SingleEventNode[T <: Txn[T], +A] extends Event.Node[T] { self =>

  def changed: Event[T, A]

  private[lucre] final def event(slot: Int): Event[T, Any] = {
    if (slot != 0) throw new IllegalArgumentException(s"Invalid slot $slot")
    changed
  }

  trait Changed extends SingleEvent[T, A] {
    def node: Event.Node[T] = self
  }
}