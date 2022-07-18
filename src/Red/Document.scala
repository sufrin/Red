package Red

/** An implementation of `DocumentInterface`.
  *
  * `textSize` is an initial size for the (expandable) characters buffer.
  *
  * `lineCount` is an initial size for the (expandable) line lineIndex.
  *
  *  The present module is a component (corresponding to his `Text`)
  *  of a refactoring and redesign of Mike Spivey's
  *  "ewoks" editor. (Copyright (c) J.M.Spivey 2015) whose
  *  inspiration I gratefully acknowledge.
  *  B.A. Sufrin, Oxford, Michaelmas 2021.
  *
  * "For present purposes, we count each newline character as part of the
  * line it terminates, so that every line has non-zero extent.  Let's
  * also imagine that a special terminator character is added to the end of
  * the characters, so that the very last line also has non-zero extent.  For an
  * ordinary characters file that ends in a newline, this last line will be empty
  * and be counted as having extent 1, and the editor will count the
  * file as having one more line than there are newline characters."
  * (J.M.Spivey, 2015)
  *
  * In this implementation, Spivey's "special" character is no longer
  * imaginary: it is represented explicitly as `END`, a character
  * that will never appear in `characters`.
  *
  */

class Document(textSize: Int=400, lineCount: Int=40)
  extends DocumentInterface {
  import Document._

  /**
   * '''Inv''' {{{ lineIndex indexes characters }}}
   *
   * (see `TextBufferIndex` for a definition of the `indexes` relation)
   */
  val characters = new TextBuffer(textSize)
  val lineIndex = new TextBufferIndex(lineCount)


  /** changes since the serveWith of the last write: read-only */
  private var _generation:      Long = 0
  @inline def generation: Long = _generation

  /** the generation of the serveWith, or of  the last write */
  private var _savedGeneration: Long = 0

  /** mark a change (insertion, or deletion) to this document */
  @inline private def change(): Unit = _generation += 1

  /** Has the document changed after loading  from the filestore
   * or since the last time it was saved
   */
  def hasChanged: Boolean = _generation != _savedGeneration

  locally {
    clear()
  }

  def coordinatesToPosition(row: Int, col: Int): Int =
    lineIndex.coordinatesToPosition(row = row, col = col)

  def positionToCoordinates(position: Int): (Int, Int) =
    lineIndex.positionToCoordinates(position)

  def clear(): Unit = {
    _generation = 0
    _savedGeneration = 0
    characters.clear()
    lineIndex.clear()
    // the last "line" has END as its single character
    lineIndex.append(1)
    characters.insert(0, Document.END)
  }

  ///////////////////////////// FUNCTIONS

  /**
   * '''Return'''
   * {{{ chars[left, right) }}}
   *
   * as a String
   * */
  def getString(left: Int, right: Int): String =
    characters.subSequence(left, right)

  /** Returns the number of lines in the sequence */
  @inline def length: Int = lineIndex.length

  /** Returns the length of the characters, excluding its `END` */
  @inline def textLength: Int = characters.length - 1

  /** Return the line at `lineNo` without its terminating newline/END */
  def getLine(lineNo: Int): String = {
    val (start, length) = lineIndex.getStartAndLength(lineNo)
    characters.subSequence(start, start + length - 1)
  }

  /** Return the fragment of the line at `lineNo` to the right
   * of `fromColumn` without its terminating newline/END
   */
  def getLine(lineNo: Int, fromColumn: Int): String = {
    val (start, length) = lineIndex.getStartAndLength(lineNo)
    if (fromColumn >= length)
      ""
    else
      characters.subSequence(start + fromColumn, start + length - 1)
  }

  ///////////////////////////// AUXILIARY MUTATIONS

  /**
   * Re-establish the invariant relation between `lineIndex` and `characters` from scratch.
   * As Mike Spivey observes, this is a fallback method, to be used in situations
   * where we decide not to update `lineIndex` incrementally.
   */
  @inline private def buildIndex(): Unit = lineIndex.buildFrom(characters)

  /**
   * Insert the characters from `source` at `position`; then expand
   * tabs in it if there are any. Return the following position.
   */

  def insert(position: Int, source: java.io.Reader, isLoad: Boolean = true): Int = {
    val originalLength = textLength
    characters.insert(position, source)
    buildIndex()
    if (isLoad) _generation = 0
    position + textLength - originalLength
  }

  /** Write the document (without its terminal `END`) to `out`.
   * TODO: decide on a policy and implementation technique
   *       for recompressing expanded tabs.
   *       One idea is to expand tabs into sequences of tabs on input; then
   *       compress such sequences to single tabs on output. It appears that
   *       most fonts show tabs as if they were single spaces....
   */
  def writeTo(out: java.io.BufferedWriter): Unit = {
    characters.writeTo(out, 1)
    _savedGeneration = _generation
  }

  /** Insert `string` at position, expanding tabs in it if necessary. Return the following position. */
  def insert(position: Int, string: String): Int = {
    var p = position
    for {ch <- string} p = insertExpanded(p, ch)
    p
  }

  /** Insert (a possibly tab-expanded)  `ch` at position. Return the following position.
   *
   * @see insertExpanded(position: Int, ch: Char): Int
   */
  @inline def insert(position: Int, ch: Char): Int = insertExpanded(position, ch)

  /** Insert (a possibly tab-expanded) `ch` at `position` and return the
   * position that follows the insertion.
   */
  private def insertExpanded(position: Int, ch: Char): Int = {
    change()
    val (line, col) = lineIndex.positionToCoordinates(position)
    ch match {
      case '\t' =>
        val n = 8 - col % 8
        // expand to the next multiple of 8
        characters.insert(position, ' ', n)
        lineIndex(line) += n
        position + n

      case '\n' =>
        characters.insert(position, '\n')
        val currentLength = lineIndex(line)
        // current line length
        lineIndex(line) = col + 1
        // rest of this line makes a new line
        lineIndex.insert(line + 1, currentLength - col)
        position + 1

      case _ =>
        characters.insert(position, ch)
        lineIndex(line) += 1
        position + 1
    }
  }

  /** Delete the character at `position` */
  def delete(position: Int): Unit = {
    change()
    val ch = characters.charAt(position)
    characters.delete(position, 1)
    val line = lineIndex.getRow(position)
    if (ch == '\n') {
      lineIndex(line) += lineIndex(line + 1) - 1
      lineIndex.delete(line + 1, 1)
    } else
      lineIndex(line) -= 1
  }

  /** Delete `count` characters at `position`
   */
  def delete(position: Int, count: Int): Unit = {
    change()
    characters.delete(position, count)
    if (lineIndex.sameLine(position, position + count))
      lineIndex(lineIndex.getRow(position)) -= count
    else
      buildIndex()
    /* TODO: note that the `lineIndex` need not be completely rebuilt
     *       because it is valid up to the "current line".
     */
  }

  /** Find the previous nonnegative `posn` in the segment
   * {{{characters[from..to]}}} for which
   * {{{
   *        bdy(character(posn-1), character(posn))
   * }}}
   * is true.
   *
   *   The function `character(i)` is identical to `characters.chars(i)`  at indexes
   *   within the document, and yields `NONE` at negative, or
   *   too-large indexes.
   */
  def findPrev(bdy: (Char,Char)=>Boolean, from: Int, to: Int): Option[Int] =
  { var posn = to
    if (logging) finer(s"findPrev($from:$to)")
    while (posn>=from  && !bdy(character(posn-1), character(posn))) posn-=1
    if (posn>=0 &&
      bdy(character(posn-1), character(posn)))
      Some(posn)
    else
      None
  }

  /**  Find the next nonnegative `posn` in the segment
   *   {{{characters[from..to]}}} for which
   *   {{{
   *        bdy(character(posn-1), character(posn))
   *   }}}
   *   is true.
   *
   *   The function `character(i)` is identical to `characters.chars(i)`
   *   at indexes within the document, and yields `NONE` at negative, or
   *   too-large indexes.
   */
  def findNext(bdy: (Char, Char) => Boolean, from: Int, to: Int): Option[Int] =
  { var posn = from +1
    if (logging) finer(s"findNext($from:$to)")
    while (posn <= to && !bdy(character(posn -1), character(posn))) posn += 1
    if (posn <= textLength && bdy(character(posn-1), character(posn)))
      Some(posn)
    else
      None
  }

  val  NONE: Char = 0.toChar

  /**  Total function equivalent to `characters.chars(i)` on indexes within the
   *   document, and yielding `NONE` at negative, or too-large indexes. This
   *   simplifies the writing of the `boundary` functions of `findNext` and
   *   `findPrev`.
   */
  def character(i: Int): Char =
    if (i <0 || i>= textLength) NONE else characters.chars(i)


  //////////////////////// Strings

  /** Return `characters` without its terminating `END` */
  override def toString: String =
    characters.subSequence(0, characters.length - 1)

  /** Return a string that discloses the representation details */
  def repString: String =
    s"${characters.repString}\n${lineIndex.repString} "
}

object Document extends Logging.Loggable {
  val  END:  Char = TextBuffer.END
}

