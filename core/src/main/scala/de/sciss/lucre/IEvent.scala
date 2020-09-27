/*
 *  IEvent.scala
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

/** In-memory (non-serializable) event. */
trait IEvent[T <: Exec[T], +A] extends Observable[T, A] {
//  private[event] def slot: Int

  /** Connects the given selector to this event. That is, this event will
    * add the selector to its propagation targets.
    */
  def ---> (sink: IEvent[T, Any])(implicit tx: T): Unit

  /** Disconnects the given selector from this event. That is, this event will
    * remove the selector from its propagation targets.
    */
  def -/-> (sink: IEvent[T, Any])(implicit tx: T): Unit

  /** Involves this event in the pull-phase of event delivery. The event should check
    * the source of the originally fired event, and if it identifies itself with that
    * source, cast the `update` to the appropriate type `A` and wrap it in an instance
    * of `Some`. If this event is not the source, it should invoke `pull` on any
    * appropriate event source feeding this event.
    *
    * @return  the `update` as seen through this event, or `None` if the event did not
    *          originate from this part of the dependency graph or was absorbed by
    *          a filtering function
    */
  private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[A]
}
