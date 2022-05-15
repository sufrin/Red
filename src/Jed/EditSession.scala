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

  /** Remove the mark
   *  (OPTIONAL PRACTICAL: cut the selection if in typeover mode)
   */
  def deSelect(): Unit = selection = NoSelection

  def insert(ch: Char): Unit = {
    deSelect()
    cursor    = document.insert(cursor, ch)
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
   *  and record the deletion as a cut.
   */
  def deleteFor(extent: Int): Unit = {
    val (l, r) = if (extent>0) (cursor, cursor+extent) else (cursor+extent, cursor)
    val text   = document.getString(l, r)
    recordCut(Cut(text, extent, cursor, document.generation))
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

 /* KEPT IN RESERVE FOR LITERAL FIND PERFORMANCE IF NEEDED
  /** pre: document.textLength>=position+thePattern.length */
  def matchesAt(pat: String, pos: Int): Boolean = {
    var last  = pat.length
    var lastc = pos+pat.length
    while ({ last -= 1; lastc -= 1; last>=0})
      if (document.character(lastc)!=pat(last))  return false
    true
  }
  /**
   *  Find the next (respectively: previous, when backwards is true) occurrence of `thePattern` after
   *  (respectively: completely before) the cursor, and position the cursor
   *  at the right (respectively left) end of the occurrence, and the mark
   *  at its opposite end. Notify via `warnings` if this fails.
   */
    def find(thePattern: String, backwards: Boolean): Boolean = {
    //  Expected time for Brute Force search is linear (in the size of the text), BUT IS NOT GUARANTEED
    //  Knuth, Morris, Pratt (see Wikipedia) is better than this in the worst case
    //
    var searching    = true
    backwards match {
      case true =>
        var position = cursor-thePattern.length
        while (searching && position >= 0)
          if (matchesAt(thePattern, position)) {
            searching = false
            // cursor at left end of selection
            cursor = position
            setMark(cursor + thePattern.length)
          } else {
            position -= 1
          }
        if (position>=0) true else { warnings.notify("Find", s"Cannot find backwards:\n  $thePattern");  false}
      case false =>
        var position = cursor
        val lastPossible = document.textLength - thePattern.length
        while (searching && position <= lastPossible)
          if (matchesAt(thePattern, position)) {
            searching = false
            // cursor at right end of selection
            cursor = position + thePattern.length
            setMark(position)
          } else {
            position += 1
          }
        if (position<=lastPossible) true else { warnings.notify("Find", s"Cannot find\n  $thePattern");  false}
    }
  }
  */

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

  /**
   * Set the cursor whilst preserving the current mark, if there is one.
   * Notify observers of changes in the selection/cursor.
   */
  def dragCursor(row: Int, col: Int): Unit = {
    if (logging)
      finer(s"DragCursor($row, $col) with selection=$selection")
    val newcursor = document.coordinatesToPosition(row, col)
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

