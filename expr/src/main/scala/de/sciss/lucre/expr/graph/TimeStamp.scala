/*
 *  TimeStamp.scala
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

package de.sciss.lucre.expr.graph

import java.util.Locale

import de.sciss.lucre.IPush.Parents
import de.sciss.lucre.TxnLike.peer
import de.sciss.lucre.expr.{Context, IAction, ITrigger, graph}
import de.sciss.lucre.impl.{IChangeEventImpl, IGeneratorEvent}
import de.sciss.lucre.{Caching, IChangeEvent, IExpr, IPull, IPush, ITargets, Txn}

import scala.concurrent.stm.{Ref, TxnLocal}

object TimeStamp {
  final case class Update(ts: TimeStamp) extends Act {
    override def productPrefix: String = s"TimeStamp$$Update" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedUpdate[T](ts.expand[T], tx)
    }
  }

//  final case class Format__(ts: TimeStamp, s: Ex[String]) extends Ex[String] {
//    override def productPrefix: String = s"TimeStamp$$Format" // serialization
//
//    type Repr[T <: Txn[T]] = IExpr[T, String]
//
//    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = ...
//  }

  final case class Format[T <: Txn[T]]() extends BinaryOp.Op[Long, String, String] {
    override def productPrefix = s"TimeStamp$$Format" // serialization

    def apply(a: Long, b: String): String =
      try {
        val f = new java.text.SimpleDateFormat(b, Locale.US)
        f.format(new java.util.Date(a))
      } catch {
        case _: IllegalArgumentException =>
          s"Invalid format '$b'"
      }
  }

//  trait Expanded[T <: Txn[T]] extends IExpr[T, Long] {
//    def update(epochMillis: Long)(implicit tx: T): Unit
//  }

  // ---- impl ----

  // XXX TODO --- perhaps use this as a general relay building block
  private final class ExpandedUpdate[T <: Txn[T]](ts: IExpr[T, Long], tx0: T)
                                                 (implicit protected val targets: ITargets[T])
    extends IAction[T] with IGeneratorEvent[T, Unit] {

    this.--->(ts.changed)(tx0)

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      if (pull.isOrigin(this)) Trig.Some
      else {
        val p: Parents[T] = pull.parents(this)
        if (p.exists(pull(_).isDefined)) Trig.Some else None
      }
    }

    def executeAction()(implicit tx: T): Unit = fire(())

    def addSource(tr: ITrigger[T])(implicit tx: T): Unit =
      tr.changed ---> this

    def dispose()(implicit tx: T): Unit =
      this.-/->(ts.changed)
  }

  private[lucre] val ref = TxnLocal(System.currentTimeMillis())

  private final class Expanded[T <: Txn[T]](tx0: T)(implicit protected val targets: ITargets[T])
    extends IExpr[T, Long] with IChangeEventImpl[T, Long] with Caching {

    // should we use universe.scheduler?
    private[this] val ref = Ref(TimeStamp.ref.get(tx0.peer))

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Long =
      if (phase.isBefore || { val p: Parents[T] = pull.parents(this); !p.exists(pull(_).isDefined) }) ref() else {
        val now = TimeStamp.ref() // System.currentTimeMillis()
        ref()   = now
        now
      }

//    private[lucre] def pullUpdateXXX(pull: IPull[T])(implicit tx: T) : Option[Change[Long]] = {
//      val p: Parents[T] = pull.parents(this)
//      if (p.exists(pull(_).isDefined)) {
//        val now     = System.currentTimeMillis()
//        val before  = ref.swap(now)
//        if (before != now) Some(Change(before, now)) else None
//      } else None
//    }

    def value(implicit tx: T): Long =
      IPush.tryPull(this).fold(ref())(_.now)

    def changed: IChangeEvent[T, Long] = this

    def dispose()(implicit tx: T): Unit = ()
  }
}
// XXX TODO --- should move to SoundProcesses and use `ExprContext.get.universe.scheduler.time` ?
final case class TimeStamp() extends Ex[Long] {
  type Repr[T <: Txn[T]] = IExpr[T, Long] // TimeStamp.Expanded[T]

  /** Creates a string representation based on `java.text.SimpleDateFormat`, US locale, and
    * default (system) time-zone.
    *
    * Within the pattern string, unquoted letters from
    * <code>'A'</code> to <code>'Z'</code> and from <code>'a'</code> to
    * <code>'z'</code> are interpreted as pattern letters representing the
    * components of a date or time string.
    * Text can be quoted using single quotes (<code>'</code>) to avoid
    * interpretation.
    * `"''"` represents a single quote.
    * All other characters are not interpreted; they're simply copied into the
    * output string during formatting or matched against the input string
    * during parsing.
    *
    * The following pattern letters are defined (all other characters from
    * <code>'A'</code> to <code>'Z'</code> and from <code>'a'</code> to
    * <code>'z'</code> are reserved):
    * <blockquote>
    * <table border=0 cellspacing=3 cellpadding=0 summary="Chart shows pattern letters, date/time component, presentation, and examples.">
    *     <tr style="background-color: rgb(204, 204, 255);">
    *         <th align=left>Letter
    *         <th align=left>Date or Time Component
    *         <th align=left>Presentation
    *         <th align=left>Examples
    *     <tr>
    *         <td><code>G</code>
    *         <td>Era designator
    *         <td>Text
    *         <td><code>AD</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>y</code>
    *         <td>Year
    *         <td>Year
    *         <td><code>1996</code>; <code>96</code>
    *     <tr>
    *         <td><code>Y</code>
    *         <td>Week year
    *         <td>Year
    *         <td><code>2009</code>; <code>09</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>M</code>
    *         <td>Month in year (context sensitive)
    *         <td>Month
    *         <td><code>July</code>; <code>Jul</code>; <code>07</code>
    *     <tr>
    *         <td><code>L</code>
    *         <td>Month in year (standalone form)
    *         <td>Month
    *         <td><code>July</code>; <code>Jul</code>; <code>07</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>w</code>
    *         <td>Week in year
    *         <td>Number
    *         <td><code>27</code>
    *     <tr>
    *         <td><code>W</code>
    *         <td>Week in month
    *         <td>Number
    *         <td><code>2</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>D</code>
    *         <td>Day in year
    *         <td>Number
    *         <td><code>189</code>
    *     <tr>
    *         <td><code>d</code>
    *         <td>Day in month
    *         <td>Number
    *         <td><code>10</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>F</code>
    *         <td>Day of week in month
    *         <td>Number
    *         <td><code>2</code>
    *     <tr>
    *         <td><code>E</code>
    *         <td>Day name in week
    *         <td>Text
    *         <td><code>Tuesday</code>; <code>Tue</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>u</code>
    *         <td>Day number of week (1 = Monday, ..., 7 = Sunday)
    *         <td>Number
    *         <td><code>1</code>
    *     <tr>
    *         <td><code>a</code>
    *         <td>Am/pm marker
    *         <td>Text
    *         <td><code>PM</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>H</code>
    *         <td>Hour in day (0-23)
    *         <td>Number
    *         <td><code>0</code>
    *     <tr>
    *         <td><code>k</code>
    *         <td>Hour in day (1-24)
    *         <td>Number
    *         <td><code>24</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>K</code>
    *         <td>Hour in am/pm (0-11)
    *         <td>Number
    *         <td><code>0</code>
    *     <tr>
    *         <td><code>h</code>
    *         <td>Hour in am/pm (1-12)
    *         <td>Number
    *         <td><code>12</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>m</code>
    *         <td>Minute in hour
    *         <td>Number
    *         <td><code>30</code>
    *     <tr>
    *         <td><code>s</code>
    *         <td>Second in minute
    *         <td>Number
    *         <td><code>55</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>S</code>
    *         <td>Millisecond
    *         <td>Number
    *         <td><code>978</code>
    *     <tr>
    *         <td><code>z</code>
    *         <td>Time zone
    *         <td>General time zone
    *         <td><code>Pacific Standard Time</code>; <code>PST</code>; <code>GMT-08:00</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>Z</code>
    *         <td>Time zone
    *         <td>RFC 822 time zone
    *         <td><code>-0800</code>
    *     <tr>
    *         <td><code>X</code>
    *         <td>Time zone
    *         <td>ISO 8601 time zone
    *         <td><code>-08</code>; <code>-0800</code>;  <code>-08:00</code>
    * </table>
    * </blockquote>
    * Pattern letters are usually repeated, as their number determines the
    * exact presentation:
    * <ul>
    * <li><strong>Text:</strong>
    *     For formatting, if the number of pattern letters is 4 or more,
    *     the full form is used; otherwise a short or abbreviated form
    *     is used if available.
    *     For parsing, both forms are accepted, independent of the number
    *     of pattern letters.<br><br></li>
    * <li><strong>Number:</strong>
    *     For formatting, the number of pattern letters is the minimum
    *     number of digits, and shorter numbers are zero-padded to this amount.
    *     For parsing, the number of pattern letters is ignored unless
    *     it's needed to separate two adjacent fields.<br><br></li>
    * <li><strong>Year:</strong>
    *     The following rules are applied.<br>
    *     <ul>
    *     <li>For formatting, if the number of pattern letters is 2, the year
    *         is truncated to 2 digits; otherwise it is interpreted as a
    *         ''number''.
    *     <li>For parsing, if the number of pattern letters is more than 2,
    *         the year is interpreted literally, regardless of the number of
    *         digits. So using the pattern "MM/dd/yyyy", "01/11/12" parses to
    *         Jan 11, 12 A.D.
    *     <li>For parsing with the abbreviated year pattern ("y" or "yy"),
    *         <code>SimpleDateFormat</code> must interpret the abbreviated year
    *         relative to some century.  It does this by adjusting dates to be
    *         within 80 years before and 20 years after the time the <code>SimpleDateFormat</code>
    *         instance is created. For example, using a pattern of "MM/dd/yy" and a
    *         <code>SimpleDateFormat</code> instance created on Jan 1, 1997,  the string
    *         "01/11/12" would be interpreted as Jan 11, 2012 while the string "05/04/64"
    *         would be interpreted as May 4, 1964.
    *         During parsing, only strings consisting of exactly two digits, as defined by
    *         `Character#isDigit(char)`, will be parsed into the default century.
    *         Any other numeric string, such as a one digit string, a three or more digit
    *         string, or a two digit string that isn't all digits (for example, "-1"), is
    *         interpreted literally.  So "01/02/3" or "01/02/003" are parsed, using the
    *         same pattern, as Jan 2, 3 AD.  Likewise, "01/02/-3" is parsed as Jan 2, 4 BC.
    *     </ul>
    *     Otherwise, calendar system specific forms are applied.
    *     For both formatting and parsing, if the number of pattern
    *     letters is 4 or more, a calendar specific `Calendar#LONG` long form is used. Otherwise, a calendar
    *     specific `Calendar#SHORT` short or abbreviated form
    *     is used.<br>
    *     <br><br></li>
    * <li><strong>Month:</strong>
    *     If the number of pattern letters is 3 or more, the month is
    *     interpreted as ''text''; otherwise,
    *     it is interpreted as a ''numbers''.<br>
    *     <ul>
    *     <li>Letter <em>M</em> produces context-sensitive month names, such as the
    *         embedded form of names.</li>
    *     <li>Letter <em>L</em> produces the standalone form of month names.</li>
    *     </ul>
    *     <br></li>
    * <li><strong>General time zone:</strong>
    *     Time zones are interpreted as ''text'' if they have
    *     names. For time zones representing a GMT offset value, the
    *     following syntax is used:
    *     <pre>
    *     <i>GMTOffsetTimeZone:</i>
    *             <code>GMT</code> <i>Sign</i> <i>Hours</i> <code>:</code> <i>Minutes</i>
    *     <i>Sign:</i> one of
    *             <code>+ -</code>
    *     <i>Hours:</i>
    *             <i>Digit</i>
    *             <i>Digit</i> <i>Digit</i>
    *     <i>Minutes:</i>
    *             <i>Digit</i> <i>Digit</i>
    *     <i>Digit:</i> one of
    *             <code>0 1 2 3 4 5 6 7 8 9</code></pre>
    *     <i>Hours</i> must be between 0 and 23, and <i>Minutes</i> must be between
    *     00 and 59. The format is locale independent and digits must be taken
    *     from the Basic Latin block of the Unicode standard.
    *     <p>For parsing, RFC 822 time zones are also
    *     accepted.<br><br></li>
    * <li><strong>RFC 822 time zone:</strong>
    *     For formatting, the RFC 822 4-digit time zone format is used:
    *
    *     <pre>
    *     <i>RFC822TimeZone:</i>
    *             <i>Sign</i> <i>TwoDigitHours</i> <i>Minutes</i>
    *     <i>TwoDigitHours:</i>
    *             <i>Digit Digit</i></pre>
    *     <i>TwoDigitHours</i> must be between 00 and 23. Other definitions
    *     are as for ''general time zones''.
    *
    *     <p>For parsing, general time zones are also
    *     accepted.
    * <li><strong>ISO 8601 Time zone:</strong>
    *     The number of pattern letters designates the format for both formatting
    *     and parsing as follows:
    *     <pre>
    *     <i>ISO8601TimeZone:</i>
    *             <i>OneLetterISO8601TimeZone</i>
    *             <i>TwoLetterISO8601TimeZone</i>
    *             <i>ThreeLetterISO8601TimeZone</i>
    *     <i>OneLetterISO8601TimeZone:</i>
    *             <i>Sign</i> <i>TwoDigitHours</i>
    *             `Z`
    *     <i>TwoLetterISO8601TimeZone:</i>
    *             <i>Sign</i> <i>TwoDigitHours</i> <i>Minutes</i>
    *             `Z`
    *     <i>ThreeLetterISO8601TimeZone:</i>
    *             <i>Sign</i> <i>TwoDigitHours</i> `:` <i>Minutes</i>
    *             `Z`</pre>
    *     Other definitions are as for ''general time zones'' or
    *     ''RFC 822 time zones''.
    *
    *     <p>For formatting, if the offset value from GMT is 0, `"Z"` is
    *     produced. If the number of pattern letters is 1, any fraction of an hour
    *     is ignored. For example, if the pattern is `"X"` and the time zone is
    *     `"GMT+05:30"`, `"+05"` is produced.
    *
    *     <p>For parsing, `"Z"` is parsed as the UTC time zone designator.
    *     ''General time zones'' are <em>not</em> accepted.
    * </ul>
    *
    * <h4>Examples</h4>
    *
    * The following examples show how date and time patterns are interpreted in
    * the U.S. locale. The given date and time are 2001-07-04 12:08:56 local time
    * in the U.S. Pacific Time time zone.
    * <blockquote>
    * <table border=0 cellspacing=3 cellpadding=0 summary="Examples of date and time patterns interpreted in the U.S. locale">
    *     <tr style="background-color: rgb(204, 204, 255);">
    *         <th align=left>Date and Time Pattern
    *         <th align=left>Result
    *     <tr>
    *         <td><code>"yyyy.MM.dd G 'at' HH:mm:ss z"</code>
    *         <td><code>2001.07.04 AD at 12:08:56 PDT</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>"EEE, MMM d, ''yy"</code>
    *         <td><code>Wed, Jul 4, '01</code>
    *     <tr>
    *         <td><code>"h:mm a"</code>
    *         <td><code>12:08 PM</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>"hh 'o''clock' a, zzzz"</code>
    *         <td><code>12 o'clock PM, Pacific Daylight Time</code>
    *     <tr>
    *         <td><code>"K:mm a, z"</code>
    *         <td><code>0:08 PM, PDT</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>"yyyyy.MMMMM.dd GGG hh:mm aaa"</code>
    *         <td><code>02001.July.04 AD 12:08 PM</code>
    *     <tr>
    *         <td><code>"EEE, d MMM yyyy HH:mm:ss Z"</code>
    *         <td><code>Wed, 4 Jul 2001 12:08:56 -0700</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>"yyMMddHHmmssZ"</code>
    *         <td><code>010704120856-0700</code>
    *     <tr>
    *         <td><code>"yyyy-MM-dd'T'HH:mm:ss.SSSZ"</code>
    *         <td><code>2001-07-04T12:08:56.235-0700</code>
    *     <tr style="background-color: rgb(238, 238, 255);">
    *         <td><code>"yyyy-MM-dd'T'HH:mm:ss.SSSXXX"</code>
    *         <td><code>2001-07-04T12:08:56.235-07:00</code>
    *     <tr>
    *         <td><code>"YYYY-'W'ww-u"</code>
    *         <td><code>2001-W27-3</code>
    * </table>
    * </blockquote>
    */
  def format(s: Ex[String]): Ex[String] =
    BinaryOp(TimeStamp.Format(), this, s)

  /** Trigger this to update the time stamp to the current time. */
  def update: Act = TimeStamp.Update(this)

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new graph.TimeStamp.Expanded[T](tx)
  }
}