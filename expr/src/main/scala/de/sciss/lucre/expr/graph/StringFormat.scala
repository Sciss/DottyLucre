/*
 *  StringFormat.scala
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

package de.sciss.lucre.expr.graph

import java.util.Locale

import de.sciss.lucre.expr.Context
import de.sciss.lucre.impl.IChangeEventImpl
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}

object StringFormat {
  private final class Expanded[T <: Txn[T]](in: IExpr[T, String], args: Seq[IExpr[T, Any]], tx0: T)
                                           (implicit protected val targets: ITargets[T])
    extends IExpr[T, String] with IChangeEventImpl[T, String] {

    args.foreach { a =>
      a.changed.--->(this)(tx0)
    }

    def value(implicit tx: T): String = {
      val inV   = in.value
      val argsV = args.map(_.value)
      tryFormat(inV, argsV)
    }

    private def tryFormat(inV: String, argsV: Seq[Any]): String =
      try {
        inV.formatLocal(Locale.US, argsV: _*)
      } catch {
        case e: IllegalArgumentException =>
          val m = e.getMessage
          s"Format error: inV - $m"
      }

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): String = {
      val inV   = pull.expr(in)
      val argsV = args.map { arg =>
        pull.expr(arg)
      }
      tryFormat(inV, argsV)
    }

//    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[String]] = {
//      val inEvt   = in.changed
//      val inChOpt = if (pull.contains(inEvt)) pull (inEvt) else None
//      val inCh    = inChOpt.getOrElse {
//        val inV = in.value
//        Change(inV, inV)
//      }
//      val argsCh = args.map { arg =>
//        val argEvt    = arg.changed
//        val argChOpt  = if (pull.contains(argEvt)) pull (argEvt) else None
//        argChOpt.getOrElse {
//          val argV = arg.value
//          Change(argV, argV)
//        }
//      }
//
//      val before  = tryFormat(inCh.before, argsCh.map(_.before))
//      val now     = tryFormat(inCh.now   , argsCh.map(_.now   ))
//      val ch      = Change(before, now)
//      if (ch.isSignificant) Some(ch) else None
//    }

    def dispose()(implicit tx: T): Unit =
      args.foreach { a =>
        a.changed.-/->(this)
      }

    def changed: IChangeEvent[T, String] = this
  }
}

/** Applies 'printf' style formatting.
  *
  * The template string may contain fixed text and one or more embedded <i>format
  * specifiers</i>. Consider the following example:
  *
  * {{{
  * val n   = "name".attr[String]("?")
  * val tmp = "Duke's name: %1\$s!"
  * PrintLn(tmp.format(n))
  * }}}
  *
  * The template
  * contains one format specifier `"%1\$s"`
  * which indicates how the argument should be processed and
  * where it should be inserted in the text. The remaining portions of the
  * template string are fixed text including `"Duke's name: "` and `"!"`.
  *
  * The argument list consists of all arguments passed to the formatter.
  * In the above example, the argument list is of size one and
  * consists of the string expression object `n`.
  *
  * <ul>
  *
  * <li> The format specifiers for general, character, and numeric types have
  * the following syntax:
  *
  * <blockquote><pre>
  * %[argument_index$][flags][width][.precision]conversion
  * </pre></blockquote>
  *
  * <p> The optional <i>argument_index</i> is a decimal integer indicating the
  * position of the argument in the argument list. The first argument is
  * referenced by `"1$"`, the second by `"2$"`, etc.
  *
  * <p> The optional <i>flags</i> is a set of characters that modify the output
  * format. The set of valid flags depends on the conversion.
  *
  * <p> The optional <i>width</i> is a positive decimal integer indicating
  * the minimum number of characters to be written to the output.
  *
  * <p> The optional <i>precision</i> is a non-negative decimal integer usually
  * used to restrict the number of characters. The specific behavior depends on
  * the conversion.
  *
  * <p> The required <i>conversion</i> is a character indicating how the
  * argument should be formatted. The set of valid conversions for a given
  * argument depends on the argument's data type.
  *
  * <li> The format specifiers which do not correspond to arguments have the
  * following syntax:
  *
  * <blockquote><pre>
  * %[flags][width]conversion
  * </pre></blockquote>
  *
  * <p> The optional <i>flags</i> and <i>width</i> is defined as above.
  *
  * <p> The required <i>conversion</i> is a character indicating content to be
  * inserted in the output.
  *
  * </ul>
  *
  * <h4> Conversions </h4>
  *
  * <p> Conversions are divided into the following categories:
  *
  * <ol>
  *
  * <li> <b>General</b> - may be applied to any argument
  * type

  * <li> <b>Numeric</b>
  *
  * <ol>
  *
  * <li> <b>Integral</b> - may be applied to integral types such as: `Int` and `Long`.
  *
  * <li><b>Floating Point</b> - may be applied to the floating-point type `Double`.
  *
  * </ol>
  *
  * <li> <b>Percent</b> - produces a literal `'%'`
  *
  * <li> <b>Line Separator</b> - produces the platform-specific line separator
  *
  * </ol>
  *
  * <p> The following table summarizes the supported conversions. Conversions
  * denoted by an upper-case character (i.e. `'B'`, `'H'`,
  * `'S'`, `'C'`, `'X'`, `'E'`, `'G'`, `'A'`, and `'T'`) are the same as those for the corresponding
  * lower-case conversion characters except that the result is converted to
  * upper case.
  *
  * <table cellpadding=5 summary="genConv">
  *
  * <tr><th valign="bottom"> Conversion
  * <th valign="bottom"> Argument Category
  * <th valign="bottom"> Description
  *
  * <tr><td valign="top"> `'b'`, `'B'`
  * <td valign="top"> general
  * <td> If <i>arg</i> is a `Boolean`, then the result is `"true"` or `"false"`.  Otherwise, the result is
  * "true".
  *
  * <tr><td valign="top"> `'s'`, `'S'`
  * <td valign="top"> general
  * <td> The result is the string representation of the argument.
  *
  * <tr><td valign="top">`'d'`
  * <td valign="top"> integral
  * <td> The result is formatted as a decimal integer
  *
  * <tr><td valign="top">`'o'`
  * <td valign="top"> integral
  * <td> The result is formatted as an octal integer
  *
  * <tr><td valign="top">`'x'`, `'X'`
  * <td valign="top"> integral
  * <td> The result is formatted as a hexadecimal integer
  *
  * <tr><td valign="top">`'e'`, `'E'`
  * <td valign="top"> floating point
  * <td> The result is formatted as a decimal number in computerized
  * scientific notation
  *
  * <tr><td valign="top">`'f'`
  * <td valign="top"> floating point
  * <td> The result is formatted as a decimal number
  *
  * <tr><td valign="top">`'g'`, `'G'`
  * <td valign="top"> floating point
  * <td> The result is formatted using computerized scientific notation or
  * decimal format, depending on the precision and the value after rounding.
  *
  * <tr><td valign="top">`'a'`, `'A'`
  * <td valign="top"> floating point
  * <td> The result is formatted as a hexadecimal floating-point number with
  * a significand and an exponent.
  *
  * <tr><td valign="top">`'%'`
  * <td valign="top"> percent
  * <td> The result is a literal `'%'` (<tt>'&#92;u0025'</tt>)
  *
  * <tr><td valign="top">`'n'`
  * <td valign="top"> line separator
  * <td> The result is the platform-specific line separator
  *
  * </table>
  *
  * <h4> Flags </h4>
  *
  * <p> The following table summarizes the supported flags.  <i>y</i> means the
  * flag is supported for the indicated argument types.
  *
  * <table cellpadding=5 summary="genConv">
  *
  * <tr><th valign="bottom"> Flag <th valign="bottom"> General
  * <th valign="bottom"> Character <th valign="bottom"> Integral
  * <th valign="bottom"> Floating Point
  * <th valign="bottom"> Date/Time
  * <th valign="bottom"> Description
  *
  * <tr><td> '-' <td align="center" valign="top"> y
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> y
  * <td> The result will be left-justified.
  *
  * <tr><td> '#' <td align="center" valign="top"> y<sup>1</sup>
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y<sup>3</sup>
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> -
  * <td> The result should use a conversion-dependent alternate form
  *
  * <tr><td> '+' <td align="center" valign="top"> -
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y<sup>4</sup>
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> -
  * <td> The result will always include a sign
  *
  * <tr><td> '&nbsp;&nbsp;' <td align="center" valign="top"> -
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y<sup>4</sup>
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> -
  * <td> The result will include a leading space for positive values
  *
  * <tr><td> '0' <td align="center" valign="top"> -
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> y
  * <td align="center" valign="top"> -
  * <td> The result will be zero-padded
  *
  * <tr><td> ',' <td align="center" valign="top"> -
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y<sup>2</sup>
  * <td align="center" valign="top"> y<sup>5</sup>
  * <td align="center" valign="top"> -
  * <td> The result will include locale-specific grouping separators
  *
  * <tr><td> '(' <td align="center" valign="top"> -
  * <td align="center" valign="top"> -
  * <td align="center" valign="top"> y<sup>4</sup>
  * <td align="center" valign="top"> y<sup>5</sup>
  * <td align="center"> -
  * <td> The result will enclose negative numbers in parentheses
  *
  * </table>
  *
  * <p> <sup>1</sup> Depends on the definition of `Formattable`.
  *
  * <p> <sup>2</sup> For `'d'` conversion only.
  *
  * <p> <sup>3</sup> For `'o'`, `'x'`, and `'X'`
  * conversions only.
  *
  * <p> <sup>4</sup> For `'d'`, `'o'`, `'x'`, and
  * `'X'` conversions applied to `BigInteger`
  * or `'d'` applied to `Int`, and `Long`.
  *
  * <p> <sup>5</sup> For `'e'`, `'E'`, `'f'`, `'g'`, and `'G'` conversions only.
  *
  * <h4> Width </h4>
  *
  * <p> The width is the minimum number of characters to be written to the
  * output.  For the line separator conversion, width is not applicable; if it
  * is provided, an exception will be thrown.
  *
  * <h4> Precision </h4>
  *
  * <p> For general argument types, the precision is the maximum number of
  * characters to be written to the output.
  *
  * <p> For the floating-point conversions `'a'`, `'A'`, `'e'`,
  * `'E'`, and `'f'` the precision is the number of digits after the
  * radix point. If the conversion is `'g'` or `'G'`, then the
  * precision is the total number of digits in the resulting magnitude after
  * rounding.
  *
  * <p> For character, integral, and date/time argument types and the percent
  * and line separator conversions, the precision is not applicable; if a
  * precision is provided, an exception will be thrown.
  *
  * <h4> Argument Index </h4>
  *
  * <p> The argument index is a decimal integer indicating the position of the
  * argument in the argument list.  The first argument is referenced by
  * `"1$"`, the second by `"2$"`, etc.
  *
  * <p> Another way to reference arguments by position is to use the
  * `'<'` (<tt>'&#92;u003c'</tt>) flag, which causes the argument for
  * the previous format specifier to be re-used.
  *
  * @param in     the template
  * @param args   the arguments to apply to the template
  */
final case class StringFormat(in: Ex[String], args: Seq[Ex[Any]]) extends Ex[String] {
  type Repr[T <: Txn[T]] = IExpr[T, String]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new StringFormat.Expanded(in.expand[T], args.map(_.expand[T]), tx)
  }
}