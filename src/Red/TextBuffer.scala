package Red

/** An extensible editable character sequence
  *
  * Represents''' {{{ chars: [Char]
  *
  * '''Representation Invariant'''
  * {{{
  *   length(chars) = super.elements.length
  *   chars(i)      = super.elements(i)
  * }}}
  *
  *  Copyright (C) B.A. Sufrin, 2021 and J.M. Spivey. 2015
  *
  *  The present module is a component of a refactoring
  *  and a redesign of the `Text`  component of Mike Spivey's
  * "ewoks" editor. (Copyright (c) J.M.Spivey 2015) whose
  *  inspiration I gratefully acknowledge. B.A. Sufrin, Oxford,
  *  Michaelmas 2021.
  */

class TextBuffer(initialSize: Int = 20)
    extends Sequence[Char](initialSize)
    with CharSequence {

  /** Construct and initialize with the given `initial: CharSequence` */
  def this(initial: CharSequence) = {
    this(initial.length)
    insert(0, initial.toString)
  }

  /** The ''i''th character of the abstraction.
    *
    *  '''Pre'''
    *  {{{0<=i<length(chars)}}}
    *  '''Return'''
    *  {{{chars(i)}}}
    */
  @inline def chars(i: Int): Char = super.elements(i)

  /** Insert all characters read from `source` into the sequence
    * at `position`, then close `source`.
    */
  def insert(position: Int, source: java.io.Reader): Unit = {
    assert(
      0 <= position && position <= length,
      s"$className.insert[$length]($position, ...)"
    )
    moveGapTo(position)
    var inserting = true
    while (inserting) {
      ensureFits(4096) // An arbitrary size
      val nread = source.read(buffer, l, gapSize)
      if (nread < 0) inserting = false
      else {
        l += nread
        length += nread
      }
    }
    source.close()
  }

  /** Write the `characters[0..characters.length-shorten)` to `out`, then close `out`. */
  def writeTo(out: java.io.BufferedWriter, shorten: Int = 0): Unit = {
    if (l > 0) out.write(buffer, 0, l)
    if (r < buffer.length) out.write(buffer, r, buffer.length - r - shorten)
    out.close()
  }

  /** Override twice-inherited isEmpty to avoid ambiguity
    *  (as insisted on by Scala 2.13).
    *  In fact both inherited `isEmpty` methods yield
    *  the same result.
    */
  override def isEmpty: Boolean = length == 0

  // ---------- Implementation of the `CharSequence` primitives

  /** '''Returns''' {{{ chars(n) }}}
    */
  def charAt(n: Int): Char = {
    assert(0 <= n && n < length, s"$className[extent=$length].charAt($n)")
    super.elements(n)
  }

  /** '''Returns''' {{{ chars[serveWith..end) }}}
    *
    * as a String. Note that `String<:CharSequence`
    */
  def subSequence(start: Int, end: Int): String = {
    assert(
      0 <= start && start <= end && end <= length,
      s"$className.subsequence[extent=$length]($start, $end)"
    )
    if (end <= l) // within the left
      new String(buffer, start, end - start)
    else if (start >= l) // within the right
      new String(buffer, start - l + r, end - start)
    else { // serveWith<l && end>l -- straddles left and right
      val result = new Array[Char](end - start)
      // copy the subsequence [serveWith..l-serveWith) lying within the left
      System.arraycopy(buffer, start, result, 0, l - start)
      // copy the subsequence [r..r+end-l) lying within the right
      System.arraycopy(buffer, r, result, l - start, end - l)
      new String(result)
    }
  }

  /** Returns `chars` as a string (no side-effects) */
  override def toString: String = subSequence(0, length)

  // ----------- Inspection methods used for small-scale testing

  /** Returns representation details as a string, with the usual whitespace
    * and end-of-characters characters made visible. (for small-scale testing
    * of tab expansion)
    */
  override def repString: String = {
    val visible = asString
      .replace(' ', '\u223c')
      .replace('\t', '\u2593')
      .replace('\n', '\u23ce')
      .replace(TextBuffer.END, '\u2588')
    s"$visible ([0..$l), [$r..${buffer.length}), $length)"
  }

  /** Returns `chars` as a string (no side effects)  with a
    * marker at the gap (for small-scale testing)
    */
  override def asString: String =
    s"${subSequence(0, l)}\u2591${subSequence(l, length)}"

}

object TextBuffer {

  /** ISO end-of-file character
    *  (but any character that cannot appear in characters will suffice)
    */
  val END: Char = ('Z' - 'A').toChar
}
