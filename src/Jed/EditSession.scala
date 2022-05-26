package Jed
import Red._

/**
 * An edit session controlling the given `document`. Usually the `path`
 * is the name of the file in the filestore from which the
 * document was loaded, and to which it is intended to be
 * saved.
 *
 * TODO: (Eliminate Code Smell) If the document was not read from the filestore,
 *       then `path` may have a different interpretation. This only
 *       happens if the document is new, or is the record of a cut ring.
 *
 * @param document
 * @param path
 */
class EditSession(val document: DocumentInterface, var path: String)
  extends Session {
  import EditSession._

  /** The document being edited here has changed since it was last
   *  loaded or written
   */
  def hasChanged: Boolean = document.hasChanged

  /** The current and previous cursor positions */
  private var _cursor, _lastCursor: Int     = 0

  /** Return the current cursor position */
  @inline def cursor: Int = _cursor

  /** Change the current cursor position, keeping
   * track of the previous position.
   */
  @inline def cursor_=(position: Int): Unit = {
    _lastCursor = _cursor
    _cursor = position
  }

  /**
   * The current and previous selections
   */
  private var _selection, _lastSelection: Span  = NoSelection

  /** Return the current selection */
  @inline def selection: Span  = _selection

  /** Change the current selection, keeping track
   * of its previous value.
   */
  def selection_=(newSelection: Span): Unit = {
    _lastSelection = _selection
    _selection = newSelection
  }

  def hasNoSelection: Boolean = _selection eq NoSelection
  def hasSelection: Boolean   = ! hasNoSelection && selectionText()!=""

  def selectionText(): String =
    document.getString(_selection.left, _selection.right)

  /**
   *   Tell handlers if the cursor or the selection has changed.
   *   The latter notification subsumes the former.
   *   This should be invoked after every user-invoked command.
   *
   *    NB: The selection-changed test is an identity test deliberately.
   *    There's a need to distinguish between `NoSelection` and a selection
   *    with cursor and mark both at 0 in the document, which signifies a
   *    dragging state started at 0.
   */
  def notifyHandlers(): Unit = {
    val notification: DocumentEvent =
    if (_selection ne _lastSelection) {
      // selection and cursor may have changed
      val (mRow, mCol) = document.positionToCoordinates(_selection.mark)
      val (cRow, cCol) = document.positionToCoordinates(this.cursor)
      SelectionChanged(mRow,mCol,_selection.extent,cRow,cCol)
    } else
    if (_cursor != _lastCursor) {
        // only the cursor has changed
        val (row, col) = document.positionToCoordinates(_cursor)
        if (logging) finer(s"cursor=${_cursor} // notifying ($row, $col)")
        CursorChanged(cursorRow=row, cursorCol=col)
    } else {
        //  neither selection nor cursor have changed, but the document may
        //  have changed -- perhaps because selected characters were replaced
        //  and the replacement reselected. At the very least the
        //  document view needs to know the cursor, because it needs to place it.
        DocumentChanged(document.positionToCoordinates(this.cursor))
    }
    if (logging) finest(s"notifyHandlers: $notification")
    notify(notification)
  }

  ////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// Positioning Command implementations ////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////

  /** Sets the cursor to `(row, col)` */
  def setCursor(row: Int, col: Int): Unit = {
    val newcursor = document.coordinatesToPosition(row, col)
    if (logging)
      finer(s"theSession.setCursor($row, $col)->$newcursor ($selection)")
    cursor = newcursor
  }

  /** Returns cursor `(row, column)` to provide feedback for a user interface if needed */
  def getCursorPosition: (Int,Int) = document.positionToCoordinates(cursor)

  /** Set the mark and change the selection. */
  def setMark(row: Int, col: Int): Unit = {
    val newPosition = document.coordinatesToPosition(row, col)
    if (logging) finer(s"theSession.setMark($row, $col)")
    setMark(newPosition)
  }

  def currentIndent: Int = {
    val (row, col) = document.positionToCoordinates(cursor)
    var startLine  = document.coordinatesToPosition(row, 0)
    var indent     = 0
    while (indent<col && document.character(startLine+indent)==' ') indent += 1
    indent
  }

  def nextTabStop: Int = {
    var (row, col) = document.positionToCoordinates(cursor)
    var indent     = col + 1
    while (indent%8!=0) indent += 1
    indent-col
  }

  /** Set the mark and change the selection. */
  def setMark(newPosition: Int, indicative: Boolean=false): Unit = {
    if (logging) finer(s"theSession.setMark($newPosition)")
    selectUntil(newPosition, indicative)
  }

  /** Change the current selection so that it spans the current `cursor`
   * position and the given `mark` position.
   */
  def selectUntil(mark: Int, indicative: Boolean = false): Unit = {
    if (logging) finer(s"selectUntil $cursor -> $mark")
    selection = Span(cursor, mark, indicative)
  }

  ///////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// Editing Command implementations ///////////////////////
  //////////////////////////////////////////////////////////////////////////////////

  var typeOverSelection: Boolean = false
  var autoIndenting:     Boolean = true

  /** Remove the mark
   *  (OPTIONAL PRACTICAL: cut the selection if in typeover mode)
   */
  def deSelect(): Unit = selection = NoSelection

  def typeOverMode: Boolean = typeOverSelection

  def insert(ch: Char): Unit = {
    deSelect()
    cursor = document.insert(cursor, ch)
  }

  def insert(string: String): Unit = {
    deSelect()
    cursor = document.insert(cursor, string)
  }

  def delete(): Unit = {
    deSelect()
    if (logging) fine(s"deleting ${document.character(cursor)}")
    deleteFor(-1)
  }

  def delete(extent: Int): Unit = {
    deSelect()
    if (logging) fine(s"deleting $cursor .. $extent")
    deleteFor(extent)
  }

  def nextLine() : Boolean = {
    deSelect()
    val (row, _) = document.positionToCoordinates(cursor)
    val newCursor = document.coordinatesToPosition(row+1, 0)
    if (cursor==newCursor) false else {
      cursor = newCursor
      true
    }
  }

  def prevLine() : Boolean = {
    deSelect()
    val (row, col) =
      document.positionToCoordinates(cursor)
    val newCursor =
      document.coordinatesToPosition(if (col==0) row-1 else row, 0)
    if (cursor==newCursor) false else {
      cursor = newCursor
      true
    }
  }

  def prevChar(): Boolean = {
    deSelect()
    if (cursor>0)
    { cursor = cursor-1
      true
    } else false
  }

  def nextChar(): Boolean = {
    deSelect()
    if (0 <= cursor && cursor<document.textLength)
    { cursor = cursor+1
      true
    } else false
  }

  /** Copy the selection, if any, to the clipboard.
   *  Returns the selected text; empty if no selection.
   */
  def copy(): String = {
    if (hasNoSelection) "" else {
      val text = selectionText()
      SystemClipboard.set(text)
      text
    }
  }

  /**
   *  Delete between cursor and cursor+extent (extent can be negative),
   *  and record the deletion as a cut, if `record`.
   */
  def deleteFor(extent: Int, record: Boolean=true): Unit = {
    val (l, r) = if (extent>0) (cursor, cursor+extent) else (cursor+extent, cursor)
    if (record) {
      val text = document.getString(l, r)
      recordCut(Cut(text, extent, cursor, document.generation))
    }
    document.delete(l, r-l)
    cursor = l
  }

  def flip(): Unit = {
    if (cursor>=2) {
      val text = document.getString(cursor-2, cursor)
      deleteFor(-2)
      insert(text.reverse)
    }
  }

  /** Cut the selection, if any, to the clipboard.
   *  Returns the selected text; empty if no selection.
   *  Record the deleted material as a cut.
   */
  def cut(): String = {
    if (hasNoSelection) "" else {
      val text = selectionText()
      SystemClipboard.set(text)
      //recordCut(Cut(text, selection.extent, selection.cursor, document.generation))
      //document.delete(selection.left, selection.right-selection.left)
      //if (selection.extent<0) cursor += selection.extent
      cursor=selection.cursor
      deleteFor(selection.extent)
      selection = NoSelection
      text
    }
  }

  /** Paste the system clipboard into the text at the
   *  cursor, and select it. Returns the previously-selected text.
   */
  def paste(): String = paste(SystemClipboard.getOrElse(""))

  /**
   *  Paste `theClip` and select it, with mark to the left.
   *  Return the previously-selected text
   */
  def paste(theClip: String): String = {
    val oldCursor = cursor
    val theText = selectionText()
    insert(theClip)
    selection = Span(cursor, oldCursor)
    theText
  }

  /**
   *  Paste `theClip` and select it, with mark to the left.
   *  Cut and return the previously-selected text.
   */
  def exch(theClip: String): String = {
    if (hasNoSelection) "" else {
      val text = selectionText()
      SystemClipboard.set(text)
      recordCut(Cut(text, selection.extent, selection.cursor, document.generation))
      document.delete(selection.left, selection.right-selection.left)
      if (selection.extent<0) cursor += selection.extent
      val oldCursor = cursor
      insert(theClip)
      selection = Span(cursor, oldCursor)
      text
    }
  }

  def toEnd(): Unit = { cursor = document.textLength }

  def toHome(): Unit = { cursor = 0 }

  def selectAll(): Unit = {
    selection = Span(0, document.textLength)
    cursor    = 0
  }

  object Bracketing {
    import gnieh.regex._
    val begin       = Brackets("""\\begin""", """\\end{[^}]+}""")
    val brace       = Brackets("""{""", """}""")
    val par         = Brackets("""\(""", """\)""")
    val bra         = Brackets("""\[""", """]""")
    val xmlblock    = Brackets("""<[A-Za-z0-9]+[^>]*>""", """</[A-Za-z0-9]+>""")
    val xmlcomment  = Brackets("""<!--""", """-->""")
    val others = List (
      "«"    -> "»",
      "⁅"    -> "⁆",
      "/\\*" -> "\\*/",
      // Adjacently coded matched pairs
      "⟦" -> "⟧", // '\u27e7'
      "⟨" -> "⟩", // '\u27e9'
      "⟪" -> "⟫", // '\u27eb'
      "⟬" -> "⟭", // '\u27ed'
      "⟮" -> "⟯", // '\u27ef'
      "⸨" -> "⸩" // '\u2e29'
    )
    val lefts, rights = collection.mutable.HashMap[Char,Brackets.Specification]()
    locally {
      for { (l, r) <- others } {
        val spec = Brackets(l, r)
        lefts.addOne((l.head, spec))
        rights.addOne((r.last, spec))
      }
    }


    def tryMatchUp(spec: Brackets.Specification): Boolean = {
      if (!spec.ket.isMatchedAtEnd(document.characters, Some(0), Some(cursor))) false else
      Brackets.matchBackward(spec, 0, cursor, document.characters) match {
        case None => false
        case Some(start) => setMark(start, true); true
      }
    }

    def tryMatchDown(spec: Brackets.Specification): Boolean = {
      if (!spec.bra.isMatchedAtStart(document.characters, Some(cursor))) false else
      Brackets.matchForward(spec, cursor, document.textLength, document.characters) match {
        case None      => false
        case Some(end) => setMark(end, true); true
      }
    }
  }

  def selectMatchingUp(): Boolean   = {
    import Bracketing._
    if (cursor==0) false else {
      document.character(cursor-1) match {
        case '}' => tryMatchUp(begin)      || tryMatchUp(brace)
        case '>' => tryMatchUp(xmlcomment) || tryMatchUp(xmlblock)
        case ')' => tryMatchUp(par)
        case ']' => tryMatchUp(bra)
        case ch  => lefts.get(ch) match {
          case Some(spec) => tryMatchUp(spec)
          case None       => false
        }
      }
    }
  }

  def selectMatchingDown(): Boolean = {
    import Bracketing._
    if (cursor>=document.textLength) false else {
      document.character(cursor) match {
        case '\\' => tryMatchDown(begin)
        case '{'  => tryMatchDown(brace)
        case '<'  => tryMatchDown(xmlcomment) || tryMatchDown(xmlblock)
        case '('  => tryMatchDown(par)
        case '['  => tryMatchDown(bra)
        case ch  => rights.get(ch) match {
          case Some(spec) => tryMatchDown(spec)
          case None       => false
        }
      }
    }
  }

  import gnieh.regex._

  object Boundaries {
    val leftWord  = Regex.composite("""\W\w""", """\w""") // the composition (\w) should be user-decideable
    val rightWord = Regex.composite("""\w\W""", """\w""") // the composition (\w) should be user-decideable
    val leftLine  = Regex.composite("\n", "\n?")
    val rightLine = Regex.composite("\n", "\n?")
    val leftPara  = Regex.composite("\n\\s*?\n", "\n?")
    val rightPara = Regex.composite("\n\\s*?\n", "\n?")
    val leftEnv   = Regex.composite("""\\begin{([^}]+)}""")
    val rightEnv  = Regex.composite("""\\end{([^}]+)}""")
  }

  /** `2 <= clicks <= 5` */
  def selectChunk(row: Int, col: Int, clicks: Int): Unit = {
    val startingCursor = document.coordinatesToPosition(row, col)

    def selectChunkMatching(left: Regex.Composite, right: Regex.Composite, adj: Int): Unit =
      left.findLastMatchIn(document.characters, 0, startingCursor) match {
        case Regex.Failure =>
        case Regex.Success(leftStart, leftEnd) =>
          right.findFirstMatchIn(document.characters, leftEnd, document.characters.length) match {
            case Regex.Failure =>
            case Regex.Success(_, rightEnd) =>
              val (start, end) = (if (leftStart==0) leftStart else leftStart + adj, rightEnd - adj)
              if (startingCursor-start > end-startingCursor) {
                cursor = end
                setMark(start)
              }
              else {
                cursor = start
                setMark(end)
              }
            }
      }
    import Boundaries._
    clicks match
    {
      case 2 => selectChunkMatching(leftWord, rightWord, 1) // word
      case 3 => selectChunkMatching(leftLine, rightLine, 1) // line
      case 4 => selectChunkMatching(leftPara, rightPara, 1) // para
      case 5 => selectChunkMatching(leftEnv, rightEnv,   0)   // \begin/end block (latex)
      case _ =>
    }
  }

  def cutAll(): String = { selectAll(); cut() }

  /**
   *  Record `thisCut` somewhere. By default this is a no-op.
   *  It is intended to be overridden by cut-ring implementations.
   *  (one such implementation is to be found in `CutRing.Plugin`)
   */
  def recordCut(thisCut: Cut): Unit = ()

  /**
   * True if this session implements a cut ring.
   */
  def hasCutRing: Boolean = false

  // Implementation of find and replace

  /** Low level methods need to be able to publish scrutable warnings  */
  val warnings: Notifier[(String,String)] = new Notifier[(String, String)]


  /**
   * Cache for most recent regex: avoids recompilation on
   * repeated find/repl.
   */
  object RegexCache {
    import gnieh.regex._
    var lastExpr: String = ""
    var lastRegex: Regex = Regex("")
    def apply(expr: String): Regex = {
      if (expr!=lastExpr) {
        lastExpr  = expr
        lastRegex = Regex(lastExpr)
      }
      lastRegex
    }
  }

  def find(thePattern: String, backwards: Boolean, asRegex: Boolean): Boolean = {
    import gnieh.regex._
    try {
      val regex = RegexCache(if (asRegex) thePattern else compiler.Parser.quote(thePattern))
      if (backwards) {
        val lastMatch = regex.findLastMatchIn(document.characters, Some(0), Some(cursor))
        lastMatch match {
          case None => false
          case Some(instance) =>
            cursor = instance.start
            setMark(instance.end)
            true
        }
      } else {
        val lastMatch = regex.findFirstMatchIn(document.characters, Some(cursor))
        lastMatch match {
          case None => false
          case Some(instance) =>
            cursor = instance.end
            setMark(instance.start)
            true
        }
      }
    } catch {
      case exn: java.lang.RuntimeException =>
        warnings.notify("Find", s"Pattern:\n  $thePattern\n is not well-formed\n ${exn.getMessage}")
        false
    }
  }

  /**  If the text, `t`, matched by the last `find` is the same as the selection:
   *      exchange it with the text formed by expanding the replacement text
   *      -- substituting `group(i)`` of the match for occurrences of `$i` in the replacement text --
   *      then return `Some(t)`.
   *
   *   Otherwise:
   *      return `None`
   */
  def replace(thePattern: String, theReplacement: String, backwards: Boolean, asRegex: Boolean) : Option[String] = try {
      import gnieh.regex._
      val lastMatch = RegexCache(if (asRegex) thePattern else compiler.Parser.quote(thePattern)).findFirstMatchIn(selectionText())
      lastMatch match {
        case None =>
             warnings.notify("Replace", s"Pattern:\n  $thePattern\n is not matched by the selection.")
             None
        case Some(instance) =>
          if (selectionText() == instance.matched.get) {
            val repl = if (asRegex) instance.substitute(theReplacement) else theReplacement
            exch(repl)
            if (backwards) {
              val c = cursor; cursor -= repl.length; setMark(c)
            }
            instance.matched
          } else {
            warnings.notify("Replace", s"Pattern:\n  $thePattern\n is not matched by the selection.")
            None
          }
      }
  } catch {
    case exn: java.lang.RuntimeException =>
      warnings.notify("Replace", s"Pattern:\n  $thePattern\n is not well-formed\n ${exn.getMessage}")
      None
  }

  def replaceAllInSelection(thePattern: String, theReplacement: String, asRegex: Boolean): Option[String] =
    try {
      import gnieh.regex._
      val regex = RegexCache(if (asRegex) thePattern else compiler.Parser.quote(thePattern))
      val selected = selectionText()
      val (count, repl) = regex.substituteAll(selected, theReplacement, !asRegex)
      warnings.notify("Replace All", s"$count replacements")
      if (count>0) { exch(repl); Some(selected) } else None
    } catch {
      case exn: java.lang.RuntimeException =>
        warnings.notify("Replace All", s"Pattern:\n  $thePattern\n is not well-formed\n ${exn.getMessage}")
        None
    }

  ///////////////////////////////////////////////////////////////////////////
  ///////////////////////// Selection by cursor-dragging ////////////////////
  ///////////////////////////////////////////////////////////////////////////


  /**
   *  Set the cursor and mark to the same position, thereby changing the selection
   *  to an empty selection at the cursor. In this state dragCursor will extend
   *  the selection.
   */
  def setCursorAndMark(row: Int, col: Int): Unit = {
    val pos = document.coordinatesToPosition(row, col)
    cursor = pos
    setMark(pos)
    if (logging)
      finer(s"setCursorAndMark($row, $col) with selection=$selection")
  }

  var draggingFrom: Option[Int] = None

  def stopDragging: Unit = { draggingFrom = None }

  /**
   * Set the cursor whilst preserving the current mark, if there is one.
   * Notify observers of changes in the selection/cursor.
   */
  def dragCursor(row: Int, col: Int): Unit = {
    if (logging)
      finer(s"DragCursor($row, $col) with selection=$selection")
    val newcursor = document.coordinatesToPosition(row, col)
    if (draggingFrom.isEmpty) draggingFrom = Some(cursor)
    cursor=newcursor
    if (selection ne NoSelection) selectUntil(selection.mark)
  }

} // EditSession

object EditSession extends Logging.Loggable


/**
 * Representation of the state of a selection in an editing session.
 * An `indicative` selection is one made to indicate a certain
 * region of text that should probably not be cut automatically
 * in insert-cuts-selection mode. It can still be cut deliberately
 * by the usual commands.
 */
case class Span(cursor: Int, mark: Int, indicative: Boolean=false) {
  // derived expressions
  @inline def markFrom(newCursor: Int): Int = newCursor + mark - cursor
  @inline def markAtRight: Boolean  = cursor<=mark
  @inline def extent: Int           = mark - cursor
  @inline def left: Int             = cursor min mark
  @inline def right: Int            = cursor max mark
  @inline def isEmpty: Boolean      = cursor==mark
}

/** An empty span used to represent no selection  */
object NoSelection extends Span(0, 0) {
  override def toString: String = "NoSelection"
}

