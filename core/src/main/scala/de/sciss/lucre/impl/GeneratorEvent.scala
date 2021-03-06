/*
 *  GeneratorEvent.scala
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

import de.sciss.lucre.Log.logEvent

trait GeneratorEvent[T <: Txn[T], A] extends Event[T, A] {
  final def fire(update: A)(implicit tx: T): Unit = {
    logEvent(s"$this fire $update")
    Push(this, update)
  }
}