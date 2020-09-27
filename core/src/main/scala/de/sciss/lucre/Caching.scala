/*
 *  Caching.scala
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

/** A marker trait for events that maintain a cache. Events mixing in this trait are
 * guaranteed to participate in the `pullUpdate` phase, even if no live observer is
 * attached to them.
 */
trait Caching
