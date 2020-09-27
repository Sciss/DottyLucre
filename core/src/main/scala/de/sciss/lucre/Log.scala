/*
 *  Log.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG

object Log {
  private lazy val eventHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'evt' ", Locale.US)

  var showEventLog = false

  @elidable(CONFIG) private[lucre] def logEvent(what: => String): Unit =
    if (showEventLog) println(eventHeader.format(new Date()) + what)

  private lazy val txnHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'stm' ", Locale.US)
  var showTxnLog = false

  @elidable(elidable.CONFIG) private[lucre] def logTxn(what: => String): Unit =
    if (showTxnLog) println(txnHeader.format(new Date()) + what)
}
