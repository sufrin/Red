package Red

import java.io.{BufferedWriter, Reader}

/** A structure that represents a sequence of "lines", in which the last "line"
  * is not necessarily terminated by a newline character. Each line is
  * considered to end with a newline character.
  *
  * The normal form of a document does not (usually) include `tab` (`'\t'`)
  * characters, which have (usually) been transformed into an appropriate number
  * of spaces when they are input from the keyboard. But if a document
  * containing such characters has been read without them being replaced by
  * other representations then they are retained.
  *
  * Copyright (C) B.A. Sufrin, 2021, J.M.Spivey 2015
  *
  * The present module is a component of a refactoring and redesign of Mike
  * Spivey's "ewoks" editor. (Copyright (c) J.M.Spivey 2015) whose inspiration I
  * gratefully acknowledge. B.A. Sufrin, Oxford, Michaelmas 2021.
  */
trait DocumentInterface {

  /** Map the (row, col) document coordinates to a position in the characters.
    */
  def coordinatesToPosition(row: Int, col: Int): Int

  /** '''Post''' {{{ characters = [] }}} */
  def clear(): Unit

  /** Insert the characters from `source` at `position`, expand tabs in it, if
    * necessary. Return the following position.
    *
    * If `isLoad` then the insertion is considered to be a load, otherwise it is
    * considered to be a change.
    */
  def insert(position: Int, source: Reader, isLoad: Boolean = true): Int

  /** Write the characters to `out` */
  def writeTo(out: BufferedWriter): Unit

  /** Insert `string` at position, expanding tabs in it if they are present.
    * Return the following position.
    */
  def insert(position: Int, string: String): Int

  /** Insert (a possibly tab-expanded) `ch` at position. Return the following
    * position.
    */
  @inline def insert(position: Int, ch: Char): Int

  /** Delete the character at `position` */
  def delete(position: Int): Unit

  /** Delete `count` characters at `position` */
  def delete(position: Int, count: Int): Unit

  /** has this document changed since it was loaded or last written? */
  def hasChanged: Boolean

  /** How many insert+delete actions have been made in this document: this
    * increases monotonically during the lifecycle of a document.
    */
  def generation: Long

  def positionToCoordinates(position: Int): (Int, Int)

  /** Find the previous nonnegative `position` in the segment
    * `characters[from..to]` for which
    * {{{boundary(character(position-1), character(position))}}} is true.
    *
    * The function `character(i)` is identical to `characters.chars(i)` at
    * indexes within the document, and yields `NONE` at negative, or too-large
    * indexes.
    */
  def findPrev(
      boundary: (Char, Char) => Boolean,
      from: Int,
      to: Int
  ): Option[Int]

  /** Find the next `position` no greater than `textLength` in the segment
    * `characters[from..to]` for which
    * {{{boundary(character(position-1), character(position))}}} is true.
    *
    * The function `character(i)` is identical to `characters.chars(i)` at
    * indexes within the document, and yields `NONE` at negative, or too-large
    * indexes.
    */
  def findNext(
      boundary: (Char, Char) => Boolean,
      from: Int,
      to: Int
  ): Option[Int]

  /** '''Return''' {{{chars[start..end}}}
    */
  def getString(start: Int, end: Int): String

  /** Returns the number of lines in the sequence */
  @inline def length: Int

  /** Returns the length of the characters, excluding its `END` */
  @inline def textLength: Int

  /** Return the line at `lineNo` without its terminating newline/END */
  def getLine(lineNo: Int): String

  /** Return the fragment of the line at `lineNo` to the right of `fromColumn`
    * without its terminating newline/END
    *
    * An efficient implementation of `getLine(lineNo).substring(fromColumn)`
    */
  def getLine(lineNo: Int, fromColumn: Int): String

  /** The "not-a-character" yielded by `character` when its index is out of
    * bounds.
    */
  val NONE: Char

  /** Total function equivalent to `characters.chars(i)` on indexes within the
    * document, and yielding `NONE` at negative, or too-large indexes.
    */
  def character(i: Int): Char

  /** The  character sequence embodied in the document */
  def characters: CharSequence

}
