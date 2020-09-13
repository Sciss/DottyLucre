package de.sciss.lucre.experiment

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG

object Log {
  private lazy val eventHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'evt' ", Locale.US)

  var showEventLog = false

  @elidable(CONFIG) private[experiment] def logEvent(what: => String): Unit =
    if (showEventLog) println(eventHeader.format(new Date()) + what)
}
