package Useful

import java.io.{StringWriter, Writer}

/**
 * A basic pretty-printing library.
 *
 * A `Format` is a linearizeable structure containing information
 * about nesting and indentation and potential line-breaks.
 *
 * The present implementation obsoletes an earlier, poorly-written,
 * library.
 */
sealed abstract class Format  {
  import Format._
  def ::(hd: Format): Format = Cons(hd, this)
  def ::(hd: String): Format = Cons(Text(hd), this)
  def :/:(hd: Format): Format = hd :: Break :: this
  def :/:(hd: String): Format = hd :: Break :: this

  def apply(n: Int): Unit = println(this.asString(n))

  /**
   * Format this document on `writer` and try to set line
   * breaks so that the result fits in `width` columns.
   */
  def format(width: Int, writer: Writer): Unit = {}

  def asString(cols: Int): String = {
    val s = new StringWriter()
    format(cols, s)
    s.toString
  }
}

object Format {

  /** The empty document */
  def empty: Format = Empty

  /** A break -- which will either be turned into a space or a line break */
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
        case ()              => Break :: d
        case text: String    => Text(text) :: d
        case doc:  Format    => doc :: d
        case other           => Text(other.toString) :: d
      }
      (ds.foldRight[Format](Empty)(dCons(_,_)))
  }

  /** A nested format whose components will (if necessary) be split and printed vertically at indentation `in`. */
  def indent(in: Int)(d: Format): Format = Indent(in, d)

  private[Format] case object Empty extends Format
  private[Format] case object Break extends Format
  private[Format] case class  Group(doc: Format) extends Format
  private[Format] case class  Indent(indent: Int, doc: Format) extends Format
  private[Format] case class  Cons(hd: Format, tl: Format) extends Format
  private[Format] case class  Text(text: String) extends Format

  def apply(ds: Any*): Format = :::(ds)

}
