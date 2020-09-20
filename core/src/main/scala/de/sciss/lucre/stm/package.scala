///*
// *  package.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre
//
//import java.text.SimpleDateFormat
//import java.util.{Date, Locale}
//
//import scala.annotation.elidable
//
//package object stm {
//  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'stm' ", Locale.US)
//  var showLog = false
//
//  @elidable(elidable.CONFIG) private[lucre] def log(what: => String): Unit =
//    if (showLog) println(logHeader.format(new Date()) + what)
//}