/*
 *  LucreConfluent.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG

object Log {
  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Confluent' -", Locale.US)
  var showLog         = false
  var showPartialLog  = false
  var showCursorLog   = false

  @elidable(CONFIG) private[confluent] def log(what: => String): Unit =
    if (showLog) Console.out.println(s"${logHeader.format(new Date())} $what")

  @elidable(CONFIG) private[confluent] def logPartial(what: => String): Unit =
    if (showPartialLog) Console.out.println(s"${logHeader.format(new Date())} partial $what")

  @elidable(CONFIG) private[confluent] def logCursor(what: => String): Unit =
    if (showCursorLog) Console.out.println(s"${logHeader.format(new Date())} cursor $what")
}
