package Jed
import Red._

class EditSession(val document: DocumentInterface, val path: String)
  extends Session {
  import  EditSession._

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
   *   TODO: [TL;DR] think again about the selection-changed test being an identity test.
   *    This was inherited from `Red`, where there's a need to distinguish between `NoSelection' and a selection
   *    with cursor and mark both at 0 in the document (of the kind that's made there during a cursor drag starting
   *    at the origin). It may not be necessary here (because we don't yet implement cursor dragging).
   *    IDEA: One possible implementation  of dragging here would be to have a "dragging" state, where drags are reported directly
   *    to the document view during dragging, and the selection is remade when the drag terminates.
   */
  def notifyHandlers(): Unit = {
    if (_selection ne _lastSelection) {
      // selection and cursor may have changed
      val (mRow, mCol) = document.positionToCoordinates(_selection.mark)
      val (cRow, cCol) = document.positionToCoordinates(this.cursor)
      notify(SelectionChanged(mRow,mCol,_selection.extent,cRow,cCol))
    } else
      if (_cursor != _lastCursor) {
        // only the cursor has changed
        val (row, col) = document.positionToCoordinates(_cursor)
        if (logging) finer(s"cursor=${_cursor} // notifying ($row, $col)")
        notify(CursorChanged(cursorRow=row, cursorCol=col))
      } else {
        //  neither selection nor cursor have changed, but the document may
        //  have changed -- perhaps because selected characters were replaced
        //  and the replacement reselected. At the very least the
        //  document view needs to know the cursor, because it
        notify(DocumentChanged(document.positionToCoordinates(this.cursor)))
      }
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

  /** Set the mark and change the selection. Notify observers of the change.  */
  def setMark(row: Int, col: Int): Unit = {
    val newPosition = document.coordinatesToPosition(row, col)
    if (logging) finer(s"theSession.setMark($row, $col)")
    setMark(newPosition)
  }


  /** Set the mark and change the selection. Notify observers of the change.  */
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
  import Commands._

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
   *  and merge the deletion with the current `sessionCut` as if it were a cut.
   */
  def deleteFor(extent: Int): Unit = {
    val (l, r) = if (extent>0) (cursor, cursor+extent) else (cursor+extent, cursor)
    val text   = document.getString(l, r)
    mergeSessionCut(Cut(text, extent, cursor, document.generation))
    document.delete(l, r-l)
    cursor = l
  }

  /** Cut the selection, if any, to the clipboard.
   *  Returns the selected text; empty if no selection.
   *  Merge the cut with the current `sessionCut`.
   */
  def cut(): String = {
    if (hasNoSelection) "" else {
      val text = selectionText()
      SystemClipboard.set(text)
      //mergeSessionCut(Cut(text, selection.extent, selection.cursor, document.generation))
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
      mergeSessionCut(Cut(text, selection.extent, selection.cursor, document.generation))
      document.delete(selection.left, selection.right-selection.left)
      if (selection.extent<0) cursor += selection.extent
      val oldCursor = cursor
      insert(theClip)
      selection = Span(cursor, oldCursor)
      text
    }
  }

  def toEnd(): Unit = cursor = document.textLength

  def toHome(): Unit = cursor = 0

  def selectAll(): Unit = {
    selection = Span(0, document.textLength)
    cursor    = 0
  }

  def cutAll(): String = { selectAll(); cut() }

  /** The most recent cut made in this session  */
  protected var sessionCut: Cut = Cut(text="", extent=0, cursor=0, generation=0L)

  /**
   *  Replace the session cut with the most recent session cut merged
   *  with this cut (if they are adjacent temporally and spatially), or
   *  just with this cut otherwise.
   *
   *  Override this method with an appropriate extension
   *  to implement a cut ring.
   */
  def mergeSessionCut(thisCut: Cut): Unit = {
    if (logging) fine(s"$sessionCut merge $thisCut".replace("\n", "\\n"))
    sessionCut = sessionCut merge thisCut
    if (logging) fine(s"$sessionCut".replace("\n", "\\n"))
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

