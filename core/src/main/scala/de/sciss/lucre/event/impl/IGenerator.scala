/*
 *  IGenerator.scala
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

package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.{Base, Exec}

trait IGenerator[T <: Exec[T], A] extends IEventImpl[T, A] {
  final def fire(update: A)(implicit tx: T): Unit = {
    logEvent(s"$this fire $update")
    IPush(this, update)(tx, targets)
  }
}