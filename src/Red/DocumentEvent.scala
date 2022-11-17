package Red

/**
 *  An `EditingSession` notifies  interested handlers, in particular
 *  `DocumentView`s, of these events. The class is sealed so that
 *  case analysis of events need not include a "catch-all" case,
 *  without which the compiler would give a spurious warning.
 */

sealed abstract class DocumentEvent

/** Something has changed within the document being edited.
 *  This is the least-specific of the events.
 *  We pretend its a change of selection.
 */

object DocumentChanged {
  def apply(cursor: (Int, Int)): DocumentEvent = new SelectionChanged(0, 0, 0, cursor._1, cursor._2, indicative = false) {
    override def toString: String = s"DocumentChanged $cursor"
  }
}

/** The cursor changed */
case class CursorChanged(cursorRow: Int, cursorCol: Int)  extends DocumentEvent

/** The position of the mark changed, thereby changing the selection.
 * `selectionSize` is  negative if the mark is above/to-the-left of the cursor
 */
case class SelectionChanged(markRow:        Int,
                            markCol:        Int,
                            selectionSize:  Int,
                            cursorRow:      Int,
                            cursorCol:      Int,
                            indicative:     Boolean
                           ) extends DocumentEvent
