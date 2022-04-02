package Red

  import Jed.Utils

  import java.awt.{Color, Graphics2D, RenderingHints}
  import javax.swing.border.MatteBorder
  import scala.swing._

  /**
   * A component that continuously displays the state of the given editing session.
   * It inherits from `InputPanel` the standard features of a swing `Panel`, and
   * its keyboard-input and mouse-input reporting style
   * It can (optionally) show a column of line-numbers at its left edge.
   *
   * @see InputPanel
   */
  class DocumentView(val theSession: Session,
                     _rows: Int=24,
                     _cols: Int=80,
                     override val font: Font=Utils.defaultFont
                    )
    extends InputPanel
  {
    import DocumentView._
    private val theDocument  = theSession.document
    private val metrics      = this.peer.getFontMetrics(font)
    private val charWidth    = metrics.charWidth('M')
    private val charHeight   = metrics.getHeight
    private val charAscent   = metrics.getAscent
    private val charDescent  = metrics.getDescent

    /** THis component is not going to show very many lines */
    private def minimalComponent: Boolean = _rows<4

    @inline private def colsToPixels(cols: Int): Int = cols * charWidth
    @inline private def rowsToPixels(rows: Int): Int = rows * charHeight


    /** Number of columns (if any) in which to write the line numbers */
    private var lineNumCols = 4
    private var lineNumberFormat = s"%0${lineNumCols}d"

    /** Set the number of columns in which to display
     *  line numbers. 0 means no display.
     */
    def viewLineNumbers(columns: Int): Unit = {
      lineNumCols = columns max 0
      lineNumberFormat = s"%0${lineNumCols}d"
      hBorder = 3*hMargin + colsToPixels(lineNumCols)
    }

    /** borders around the characters */
    private val hMargin = if (minimalComponent) 2 else 5
    private val vBorder = if (minimalComponent) 0 else 5
    private var hBorder = 3*hMargin + colsToPixels(lineNumCols)
    def hborder_=(px: Int): Unit = hBorder=px

    /** current number of rows and columns of the view */
    private var rows, cols: Int      = 0
    /** current width of the characters display */
    private var textWidthPixels: Int = 0
    /** current mark column, row, and size (negative if cursor is at the right) */
    private var markedCol, markedRow, markedTextLength = 0
    /** current cursor column and row */
    private var cursorCol, cursorRow = 0
    /** current location within the document of the top-left corner of the view  */
    private var originCol, originRow = 0

    private val borderSize = if (minimalComponent) 3 else 5

    /** Border when the view has the focus  */
    private val focussedBorder   = new MatteBorder(borderSize,borderSize,borderSize,borderSize, Color.gray)
    /** Border when the view doesn't have the focus  */
    private val unfocussedBorder = new MatteBorder(borderSize,borderSize,borderSize,borderSize, Color.lightGray)
    // if (!minimalComponent)
      border=focussedBorder

    /** quanta for recomputing the origin */
    private val colDrift = 10
    private val defaultRowDrift = 4

    /**
     *   Calculate a new origin that keeps the new cursor visible. The
     *   algorithm is very straightforward. However it can lead to
     *   "disappointingly small" vertical jumps for a user who is
     *   navigating the characters with arrow keys. But the obvious
     *   solution (increasing the row drift) may result in disproportionately
     *   large origin jumps that infringe the principle of least surprise, and
     *   require the user to search by eye for the position of the cursor.
     *
     *   TODO: (practical) experiment with solving both problems at once.
     *         HINT: take the previous value of the cursor into account
     */
    def recomputeOrigin(newCursorRow: Int, newCursorCol: Int): Unit = {
      val rowDrift = if (Math.abs(newCursorRow-cursorRow)>rows) 3*defaultRowDrift else defaultRowDrift
      if (newCursorRow < originRow) originRow = (newCursorRow - rowDrift) max 0
      while (newCursorRow >= originRow + rows) originRow += rowDrift
      // column origin is "elasticated": snaps to 0 if it can
      if (newCursorCol<cols) originCol = 0  else {
        while (newCursorCol < originCol + cols && originCol >= colDrift)
          originCol -= colDrift
        while (newCursorCol > originCol + cols)
          originCol += colDrift
      }
    }

    override def paintComponent(g: Graphics2D): Unit = {
      // Make the characters smoother, if possible
      g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON
      )
      calculateDimensions()
      if (markedTextLength!=0) calculateSelectionExtent()

      // clear the background
      g.setColor(background)
      g.fillRect(0, 0, size.width, size.height)
      // Demarcate the characters
      g.setColor(if (hasFocus) Color.lightGray else Color.white)
      g.fill3DRect(0, 0, size.width, 2, true)
      // ... and the line numbers (vertical)
      if (lineNumCols > 0) {
        g.fill3DRect(hBorder - hMargin, 1, 2, size.height, hasFocus)
      }

      //if (!minimalComponent)
         border = if (hasFocus) focussedBorder else unfocussedBorder

      // indicate if the originCol is shifted horizontally
      if (originCol > 0) {
        g.setColor(Color.green)
        g.fillRect(hBorder - 2, 0, 2, size.height)
      }

      g.setColor(foreground)

      // draw the visible characters
      for { row <- 0 until (rows min theDocument.length)
            // DECLARE documentRow [Scala 2.13 deprecates the val]
            /*val*/ documentRow=row+originRow
            if documentRow<theDocument.length
          } {

        val baseLine = vBorder + (1 + row) * charHeight

        // draw the line number if required
        if (lineNumCols > 0) {
          g.setColor(Color.lightGray)
          g.drawString(lineNumberFormat.format(documentRow+1),
                       hMargin, baseLine)
          g.setColor(foreground)
        }

        val theLine = theDocument.getLine(documentRow, originCol)
        // draw the background for this line (darker for the selection)
        if (markedTextLength!=0)
           drawBackground(g, documentRow, baseLine, theLine.length)
        // Draw the line itself
        g.drawString(theLine, hBorder, baseLine)
      }

      // draw the cursor if it's visible
      if (logging) finest(s"cursor=($cursorRow,$cursorCol)")
      if (isVisible(cursorRow, cursorCol))
        drawCursorShape(g,
          if (hasFocus) Color.red
          else Color.darkGray, cursorRow, cursorCol, 3)

      // draw the mark, if it's visible: it's a tad narrower than the cursor
      if (markedTextLength!=0 && isVisible(markedRow, markedCol))
        drawCursorShape(g, Color.blue, markedRow, markedCol, 2)
    }

    /**
     * Draw a cursor shape.
     *
     * Make a visible fuss if the component is too narrow to show
     * it sensibly..
     */
    def drawCursorShape(g: Graphics2D, color: Color, row: Int, col: Int, width: Int): Unit = {
        val rectX = colsToPixels (col - originCol)
        val rectW = if (rectX < 0) 6 else width
        g.setColor(color)
        g.fillRect(hBorder + (0 max rectX) - 2,
                   vBorder + charDescent + rowsToPixels(row-originRow),
                   rectW,
                   charHeight
                  )
    }


    /**
     * Key locations in the selectin
     *
     * 1 The selection is on lines between `selectionTop` and `selectionBottom`.
     *
     * 2 The selection is to the right of `selectionLeft` on `selectionTop`.
     *
     * 3 The selection is to the left of `selectionRight` on `selectionBottom`.
     *
     */
    var selectionTop, selectionBottom, selectionLeft, selectionRight = 0

    /**
     * Calculate the extent of the selection
     */
    def calculateSelectionExtent(): Unit = if (markedTextLength!=0) {
      if (cursorRow<markedRow) {
        selectionTop=cursorRow
        selectionLeft=cursorCol
        selectionBottom=markedRow
        selectionRight=markedCol
      } else if (cursorRow>markedRow) {
        selectionTop=markedRow
        selectionLeft=markedCol
        selectionBottom=cursorRow
        selectionRight=cursorCol
      } else {
        selectionTop    = cursorRow
        selectionBottom = cursorRow
        selectionLeft   = cursorCol min markedCol
        selectionRight  = cursorCol max markedCol
      }
    }

    /**
     * Draw the background for row if the selection
     * appears wholly or partly on it.
     *
     * '''Pre''' there is a selection
     */
    private
    def drawBackground(g: Graphics2D,
                       documentRow: Int,
                       baseLine: Int,
                       textWidth: Int): Unit =
    {
      /* We are going to draw a rectangle on the baseline with
       * left edge and width, determined by which line of the
       * selection is being drawn.
       */
      if (markedTextLength != 0           &&
          selectionTop     <= documentRow &&
          documentRow      <= selectionBottom)
      { // This seems inelegant, but might not be much improved
        // by abstracting the fillRect invocation. Left as an exercise.
        var left  = 0
        var width = 0
        if (documentRow == selectionTop) {
          left = 0 max colsToPixels(selectionLeft - originCol)
          if (selectionTop == selectionBottom)
            width = colsToPixels(selectionRight - selectionLeft)
          else
            width = colsToPixels(textWidth) - left
        }
        else
        if (documentRow == selectionBottom)
          width = colsToPixels(selectionRight - originCol)
        else
          width = colsToPixels(textWidth-originCol)
        g.setColor(Color.lightGray)
        g.fillRect(hBorder + left, baseLine - charAscent, width, charHeight)
        g.setColor(foreground)
      }
    }

    @inline def isVisible(row: Int, col: Int): Boolean =
      originRow <= row && row <= originRow + rows &&
      originCol <= col && col <= originCol + cols

    /**
     * Calculate the rectangular dimensions, in character units, of the display.
     * Calculate the width in pixels available for characters.
     * The initial actual `size` may not be that set by `defaultSize()`, because
     * the context in which the view is "packed" by the layout manager(s)
     * may force it to be larger than that.
     */
    private def calculateDimensions(): Unit =  {
      val (w, h) = (size.width - (hBorder+hMargin), size.height - 2 * vBorder)
      rows = h/charHeight
      cols = w/charWidth
      textWidthPixels = colsToPixels(cols)
    }

    /**
     * Dimensions needed for a `cols,_rows` view. The
     * actual size will depend on the context in which
     * the view is placed.
     */
    def defaultSize(): Dimension =
       new Dimension(hBorder+colsToPixels(_cols),
                     2*vBorder+rowsToPixels(_rows) + charDescent)


    /**
     *   Calculate document location (row, col) from absolute
     *   mouse coordinates (x, y).
     *
     *   We consider the logical cursor to be "on" the character
     *   just to its right but in calculating the mapping from
     *   pixel to column number we allow (`sloppyX`) for the fact
     *   that (most) users place the cursor somewhere inside
     *   the visible glyph.
     */
    private def cursorToDocument(cursor: Point): (Int, Int) = {
      calculateDimensions()
      val (y, x) = (cursor.y, cursor.x)
      val sloppyX = x + charWidth/2
      val row = originRow + (y - vBorder) / charHeight
      val col = originCol + (sloppyX - hBorder) / charWidth

      if (logging)
        finest(s"cursorToDocument($cursor) => $row, $col)")

      (row, col)
    }

    /** Map view location to model location: same as `cursorToDocument`.  */
    protected def viewToModel(pointerLocation: Point): (Int, Int) =
      cursorToDocument(pointerLocation)

    /** Respond to this component changing size  */
    protected def componentResized(): Unit = {
      calculateDimensions()
      recomputeOrigin(cursorRow, cursorCol)
      if (logging) finer(s"$rows x $cols")
      repaint()
    }

    /** Respond to `MouseWheel` events by scrolling.
      * There is no need to inform the session.
      */
    def mouseWheel(rotation: Int, mods: InputEvent.Detail): Unit = {
        if (logging) info(s"wheel $rotation $mods")
        calculateDimensions()
        val newRow = (originRow + rotation) max 0
        if (newRow<originRow || newRow + rows <= theDocument.length + rows / 4) {
          originRow = newRow
          repaint()
        }
    }

    locally {
      calculateDimensions()
      // tell the session about our handler for document events
      theSession.handleWith(handleDocumentEvent(_))
      //
      focusable = true
    }

    /** Handler for `DocumentEvent`s notified by the editing session */
    def handleDocumentEvent(event: DocumentEvent): Unit = event match {

      case CursorChanged(row, col) =>
        recomputeOrigin(row, col)
        this.cursorRow = row
        this.cursorCol = col
        if (logging) finer(s"CursorChanged: ($row, $col)")
        /* TODO: it would be possible to refine this
         *  for situations when the original and current cursor
         *  lines are in the same place on the screen, and they are
         *  the only ones needing to be redrawn. But
         *  for the moment we will always repaint everything
         *  that's visible since that's efficient enough and much simpler.
         */
        repaint()

      case SelectionChanged(markedRow, markedCol, size, cursorRow, cursorCol) =>
        recomputeOrigin(cursorRow, cursorCol)
        this.markedRow  = markedRow
        this.markedCol  = markedCol
        this.markedTextLength = size
        this.cursorRow = cursorRow
        this.cursorCol = cursorCol
        if (logging)
          finer(s"SelectionChanged: ($markedRow, $markedCol)" +
            s"-->($cursorRow, $cursorCol) ($size chars)")
        /* TODO: it would be possible to refine this
         *  for situations when the scope of the
         *  selection change has remained static on the screen. But
         *  for the moment we will always repaint everything that's visible
         *  since that's efficient enough and much simpler.
         */
        repaint()
    }

  } // DocumentView

  object DocumentView extends Logging.Loggable


