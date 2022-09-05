package Red

object FilterUtilities {
  import scala.annotation.tailrec

  /** Parse the given string as a sequence
    * of strings suitable for use as arguments
    * to (external) processes.
    *
    * The parse is straightforward: arguments are
    * normally separated by one or more spaces, but
    * more complex arguments may be quoted (with ', or ")
    * or contain spaces or quotes "quoted" by preceding
    * them with a backslash.
    *
    * Comments extend from an unquoted `#` to
    * the end of the string.
    *
    * A "nested" argument starts with '{'
    * and extends to the next matching '}'
    * character (or the end of the string).
    * It includes the brace characters
    * themselves.
    * {{{
    *   foo bar       => ["foo", "bar"]
    *   "foo bar"     => ["foo bar"]
    *   foo\ bar      => ["foo bar"]
    *   foo\'bar      => ["foo'bar"]
    *   {foo bar}     => ["{foo bar}"]
    *   {foo {x} bar} => ["{foo {x} bar}"]
    * }}}
    */

  def parseArguments(parameter: CharSequence): List[String] =
    parse(parameter.toString.toList, List(), List())

  /** Make an argument string from the reversed list of its constituent characters */
  private def mkArg(revArg: List[Char]): String =
    revArg.reverse.mkString("")

  /** Parse, from `chars`, a list of (string) arguments whose first
    *  string is expected to end with `unquote`.
    *
    *  @param chars the list of characters to be parsed
    *  @param revArg the (reversed) list of characters comprising the
    *                currently-to-be-parsed argument
    *  @param revResult the (reversed) list of arguments that have already been parsed
    *
    *  If no `unquote` appears, then the first string argument extends
    *  to the end of `chars`.
    */
  @tailrec private def parseQuoted(
      unquote: Char,
      chars: List[Char],
      revArg: List[Char],
      revResult: List[String]
  ): List[String] =
    chars match {
      case ch :: rest if ch == unquote =>
        parse(rest, List(), mkArg(revArg) :: revResult)
      case '\\' :: ch :: rest if ch == ' ' || ch == '"' || ch == '\'' =>
        parseQuoted(unquote, rest, ch :: revArg, revResult)
      case ch :: rest =>
        parseQuoted(unquote, rest, ch :: revArg, revResult)
      case _ =>
        (if (revArg.isEmpty) revResult else mkArg(revArg) :: revResult).reverse
    }

  @tailrec private def parseNested(
      level: Int,
      quote: Char,
      unquote: Char,
      chars: List[Char],
      revArg: List[Char],
      revResult: List[String]
  ): List[String] =
    chars match {
      case ch :: rest if ch == unquote =>
        if (level == 1)
          parse(rest, List(), s"$quote${mkArg(revArg)}$unquote" :: revResult)
        else
          parseNested(level - 1, quote, unquote, rest, ch :: revArg, revResult)
      case '{' :: rest =>
        parseNested(level + 1, quote, unquote, rest, '{' :: revArg, revResult)
      case ch :: rest =>
        parseNested(level, quote, unquote, rest, ch :: revArg, revResult)
      case _ =>
        (if (revArg.isEmpty) revResult else mkArg(revArg) :: revResult).reverse
    }

  /**  Parse, from `chars`, a list of (string) arguments.
    *
    *  @param chars the list of characters to be parsed
    *  @param revArg the (reversed) list of characters comprising the
    *                currently-to-be-parsed argument
    *  @param revResult the (reversed) list of arguments that have already been parsed
    */
  @tailrec private def parse(
      chars: List[Char],
      revArg: List[Char],
      revResult: List[String]
  ): List[String] =
    chars match {
      case quote :: rest if quote == '\'' || quote == '"' =>
        parseQuoted(quote, rest, revArg, revResult)
      case '{' :: rest =>
        parseNested(1, '{', '}', rest, revArg, revResult)
      case '\\' :: ch :: rest if ch == ' ' || ch == '"' || ch == '\'' =>
        parse(rest, ch :: revArg, revResult)
      case ' ' :: rest =>
        if (revArg.isEmpty)
          parse(rest, revArg, revResult)
        else
          parse(rest, List(), mkArg(revArg) :: revResult)
      case '#' :: _ =>
        (if (revArg.isEmpty) revResult else mkArg(revArg) :: revResult).reverse
      case ch :: rest =>
        parse(rest, ch :: revArg, revResult)
      case _ =>
        (if (revArg.isEmpty) revResult else mkArg(revArg) :: revResult).reverse
    }

  import java.io.{ByteArrayInputStream, InputStream}

  private val UTF8: String = java.nio.charset.StandardCharsets.UTF_8.name

  /** Construct an input stream from a UTF8 string (often for use by a filter) */
  def inputStreamOf(input: String): InputStream =
    new ByteArrayInputStream((if (input == "") " " else input).getBytes(UTF8))
}
