package Useful

import java.io.{StringWriter, Writer}
import scala.annotation.tailrec

/**
 * A basic pretty-printing library, based on Lindig's strict version
 * of Wadler's adaptation of Hughes' pretty-printer.
 *
 * A `Format` is a linearizeable structure containing information
 * about nesting and indentation and potential line-breaks.
 */
sealed abstract class Format  {
  import Format._
  def ::(hd: Format): Format = Cons(hd, this)
  def ::(hd: String): Format = Cons(Text(hd), this)
  def :/:(hd: Format): Format = hd :: Break :: this
  def :/:(hd: String): Format = hd :: Break :: this

  /**
   * Format this document on `writer` and try to set line
   * breaks so that the result fits in `width` columns.
   */
  def format(width: Int, writer: Writer): Unit = {
    type FormatState = (Int, Boolean, Format)

    @tailrec
    def fits(w: Int, state: List[FormatState]): Boolean = state match {
      case _ if w < 0 =>
        false
      case List() =>
        true
      case (_, _, Empty) :: z =>
        fits(w, z)
      case (i, b, IndentBy(ii, d)) :: z => false // forces indentation
      case (i, b, Cons(h, t)) :: z =>
        fits(w, (i, b, h) :: (i, b, t) :: z)
      case (_, _, Text(t)) :: z =>
        fits(w - t.length(), z)
      case (i, b, Nest(ii, d)) :: z =>
        fits(w, (i + ii, b, d) :: z)
      case (_, false, Break) :: z =>
        fits(w - 1, z)
      case (_, true, Break) :: _ =>
        true
      case (i, _, Group(d)) :: z =>
        fits(w, (i, false, d) :: z)
    }

    def spaces(n: Int): Unit = {
      for { i <- 0 until n } { writer write " " }
    }

    @tailrec
    def fmt(k: Int, state: List[FormatState]): Unit = state match {
      case List() => ()
      case (_, _, Empty) :: z =>
        fmt(k, z)
      // Render the first of an indented sequence. See `Nest`
      case (i, _, IndentBy(ii, t)) :: z =>
        spaces(ii)
        fmt(k, (i+ii, true, t) :: z)
      case (i, b, Cons(h, t)) :: z =>
        fmt(k, (i, b, h) :: (i, b, t) :: z)
      case (i @ _, _, Text(t)) :: z =>
        writer write t
        fmt(k + t.length(), z)
      case (i, b, Nest(ii, d)) :: z =>
        val fitsFlat = fits(width - k, (i, false, d) :: z)
        // println(s"fits(${width-k}, ${(i, false, d) :: z})=$fitsFlat")
        if (true)
          fmt(k, (i+ii, b, d) :: z)
        else
        if (fitsFlat)
           fmt(k, (i, false, d) :: z)
        else
           fmt(k, (i, true, IndentBy(ii, d))::z)
      case (i, true, Break) :: z =>
        writer write "\n"
        spaces(i)
        fmt(i, z)
      case (i @ _, false, Break) :: z =>
        writer write " "
        fmt(k + 1, z)
      case (i, b @ _, Group(d)) :: z =>
        val fitsFlat = fits(width - k, (i, false, d) :: z)
        fmt(k, (i, !fitsFlat, d) :: z)
      case _ =>
        ()
    }

    fmt(0, List((0, false, Group(this))))
  }

  def asString(cols: Int): String = {
    val s = new StringWriter()
    format(cols, s)
    s.toString
  }
}

object Format {

  /** The empty document */
  def empty: Format = Empty

  /** A break, which will either be turned into a space or a line break */
  def break: Format = Break

  /** A document consisting of some text literal */
  def text(s: String): Format = Text(s)

  /**
   * A group, whose components will either be printed with all breaks
   * rendered as spaces, or with all breaks rendered as line breaks.
   */
  def group(d: Format): Format = Group(d)

  /**
   * A format
   */
  def :::(ds: Any*): Format = {
      def dCons(a: Any, d: Format): Format = a match {
        case ()                    => Break :: d
        case (n: Int, nd: Format)  => nest(n)(nd) :: d
        case ds:   Seq[Any]  => ds.foldRight[Format](Empty)(dCons(_,_)) :: d
        case text: String    => Text(text) :: d
        case doc:  Format    => doc :: d
        case other           => Text(other.toString) :: d
      }
      (ds.foldRight[Format](Empty)(dCons(_,_)))
  }

  /** A nested format whose components will (if necessary) be split and printed vertically at indentation `in`. */
  def nest(in: Int)(d: Format): Format = Nest(in, d)

  /** A nested format whose components will ''always'' be split and printed vertically at indentation `in`.  */
  def ind(in: Int)(d: Format): Format = IndentBy(in, d)

  private[Format] case class  IndentBy(indent: Int, doc: Format) extends Format
  private[Format] case object Empty extends Format
  private[Format] case object Break extends Format
  private[Format] case class  Text(txt: String) extends Format
  private[Format] case class  Group(doc: Format) extends Format
  private[Format] case class  Nest(indent: Int, doc: Format) extends Format
  private[Format] case class  Cons(hd: Format, tl: Format) extends Format

  def apply(ds: Any*): Format = :::(ds)

}
