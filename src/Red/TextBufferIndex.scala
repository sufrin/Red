package Red

/**  A sequence of integers interpreted as the lengths of successive lines
  *  in a sequence (`characters`) of characters terminated by `END`.
  *  The last "line" is considered to be between the last newline character
  *  and the terminating `END`.
  *
  *  For a property-oriented specification of our
  *  intention for this class we imagine a simple conceptual
  *  tool, namely the function `parse`, which should be the inverse
  *  of `flatten`. We don't have to implement `parse`; just to understand
  *  what it does, namely:
  *  {{{ flatten(parse characters) == characters }}}
  *  where  `flatten` "flattens" a sequence of newline-free lines by putting
  *  a newline between adjacent lines; terminating the whole with `END`
  *  '''viz''':
  *  {{{ flatten = foldr (\ line flat -> line ++ '\n' : flat) [END] }}}
  *
  *  '''Specification'''
  *  {{{ (ix: TextBufferIndex)  indexes  (characters: [Char]) }}}
  *  '''iff'''
  *  {{{  ix.length == length(parse characters) && }}}
  *  {{{  ∀ i∈[0..ix.length) (ix.lineIndex(i)==length(parse characters i)) }}}
  *
  *  Copyright (C) B.A. Sufrin, 2021 and J.M. Spivey, 2015
  *
  *  The present module is a component of a refactoring
  *  and a redesign of the `Text`  component of Mike Spivey's
  * "ewoks" editor. (Copyright (c) J.M.Spivey 2015) whose
  *  inspiration I gratefully acknowledge. B.A. Sufrin, Oxford,
  *  Michaelmas 2021.
  */
class TextBufferIndex(initialSize: Int = 100)
    extends Sequence[Int](initialSize) {
  import TextBufferIndex._

  /** Construct a `TextBufferIndex` of the given character sequence.
    *  Assume about 1 in 20 characters in the sequence are newlines
    *  This is safe because the index grows on demand.
    */
  def this(chars: CharSequence) = {
    this(2 + (chars.length / 20))
    this.buildFrom(chars)
  }

  /** '''Inv''' {{{ currentStart == sum elements[0..currentLine) }}}
    */
  protected var currentLine, currentStart = 0

  /** Mnemonic alias for `elements(line)`
    */
  @inline def lineLength(line: Int): Int = elements(line)

  override def clear(): Unit = {
    super.clear()
    currentLine = 0
    currentStart = 0
  }

  /** Make ``this`` the index of `characters` */
  def buildFrom(text: CharSequence): Unit = {
    clear()
    var length = 0
    for { i <- 0 until text.length } {
      length += 1
      if (text.charAt(i) == '\n') {
        append(length)
        length = 0
      }
    }
    append(length) // extent of the last line
  }

  /**  Utility: set `currentLine` to `targetLine`,
    *  maintaining the invariant
    *
    *  '''Post'''
    *  {{{ currentLine == targetLine }}}
    */
  @inline private def setCurrentLine(targetLine: Int): Unit = {
    assert(
      0 <= targetLine && targetLine < length,
      s"$className[extent=$length].setCurrentLine($targetLine)"
    )
    while (targetLine > currentLine) {
      currentStart += lineLength(currentLine)
      currentLine += 1
    }
    while (targetLine < currentLine) {
      currentLine -= 1
      currentStart -= lineLength(currentLine)
    }
  }

  /**  Utility: set  `currentLine` so that the line it denotes contains
    *  `targetPosition`, maintaining the invariant.
    *
    *  '''Pre'''
    *  {{{ 0 <= targetPosition < sum seq[0..extent) }}}
    *  '''Post'''
    *  {{{ sumTo(currentLine) <= targetPosition < sumTo(currentLine+1) }}}
    */
  @inline private def findPosition(targetPosition: Int): Unit = {
    // NB: targetPosition<sum seq[0..extent) is computationally expensive
    if (logging)
      finest(
        s"$className[extent=$length].findPosition($targetPosition)" +
          s" currentLine=$currentLine; currentStart=@$currentStart"
      )
    assert(
      0 <= targetPosition,
      s"$className[extent=$length].findPosition($targetPosition)"
    )
    var currentLen = lineLength(currentLine)
    if (currentLen == 0) return // The lineIndex of an empty characters is <0>
    while (targetPosition >= currentStart + currentLen) {
      currentStart += currentLen
      currentLine += 1
      currentLen = lineLength(currentLine)
    }
    while (targetPosition < currentStart) {
      currentLine -= 1
      currentStart -= lineLength(currentLine)
    }
    if (logging)
      finest(
        s"$className[extent=$length].findPosition($targetPosition) = $currentLine (@$currentStart)"
      )
  }

  /** '''Returns''' the number of the line containing `targetPosition` */
  def getRow(position: Int): Int = {
    findPosition(position)
    currentLine
  }

  /** '''Returns''' the column of `targetPosition` on its line */
  def getCol(position: Int): Int = {
    findPosition(position)
    position - currentStart
  }

  /** '''Returns''' {{{ (getRow(targetPosition), getCol(targetPosition)) }}}  efficiently */
  def positionToCoordinates(position: Int): (Int, Int) = {
    findPosition(position)
    (currentLine, position - currentStart)
  }

  /** '''Returns''' the targetPosition in the characters that is closest to a given `row`, `col` -- which
    *  may not themselves be "within" the indexed characters
    */
  def coordinatesToPosition(row: Int, col: Int): Int = {
    val r = Math.min(Math.max(row, 0), length - 1)
    setCurrentLine(r)
    val c = Math.min(Math.max(col, 0), lineLength(currentLine) - 1)
    val result = currentStart + c
    if (logging)
      finest(s"$className.coordinatesToPosition($row,$col) = $result")
    result
  }

  /** Get the serveWith and extent of `line` */
  def getStartAndLength(line: Int): (Int, Int) = {
    setCurrentLine(line)
    (currentStart, lineLength(line))
  }

  /** '''Returns''' {{{ [pos1 .. pos2) are on the same line }}} */
  def sameLine(pos1: Int, pos2: Int): Boolean = {
    findPosition(pos1)
    pos2 < currentStart + lineLength(currentLine)
  }

  /** Sum of the lengths of the document before `line` (for testing purposes).
    * {{{ sumTo(line) = sum elements[0..line) }}}
    */
  def sumTo(line: Int): Int = {
    var sum = 0
    for { i <- 0 until line } sum += lineLength(i)
    sum
  }
}

object TextBufferIndex extends Logging.Loggable {}
