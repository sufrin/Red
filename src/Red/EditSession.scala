package Red

import sufrin.regex.Brackets.Specification
import sufrin.regex.Regex
import sufrin.regex.syntax.{Alt, Literal, Tree}

import java.nio.file.Path

/**
 *
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
class EditSession(val document: DocumentInterface, private var _path: String)
  extends DocumentProvider {
  import EditSession._

  /** the document being shown here is ephemeral, so doesn't need saving. */
  private var _ephemeral: Boolean   = false
  def makeEphemeral(): Unit = _ephemeral=true

  /** The document being edited here has changed since it was last
   *  loaded or written
   */
  def hasChanged: Boolean = ! _ephemeral && document.hasChanged

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

  def hasNoSelection: Boolean         = _selection eq NoSelection
  def hasSelection: Boolean           = ! hasNoSelection && selectionText()!=""
  def hasDefiniteSelection: Boolean   = ! hasNoSelection && ! selection.tentative

  def selectionText(): String =
    document.getString(_selection.left, _selection.right)

  /**
   * True when a refresh has to be forced: for the moment this
   * is only after an autoindenting newline is invoked when there's
   * already a selection.
   */
  var forceRefresh: Boolean = false

  /**
   *   Tell handlers if the cursor or the selection has changed.
   *   The latter notification subsumes the former.
   *   This should be invoked after every user-invoked command.
   *
   *    NB: The selection-changed test is an identity test deliberately.
   *    There's a need to distinguish between `NoSelection` and a selection
   *    with cursor and mark both at 0 in the document. The latter signifies a
   *    dragging state started at 0.
   */
  def notifyHandlers(): Unit = {
    val notification: DocumentEvent =
    if (forceRefresh || ( _selection ne _lastSelection)) {
      // selection and cursor may have changed
      val (mRow, mCol) = document.positionToCoordinates(_selection.mark)
      val (cRow, cCol) = document.positionToCoordinates(this.cursor)
      forceRefresh = false
      SelectionChanged(mRow,mCol,_selection.extent,cRow,cCol, _selection.tentative)
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
    if (logging) {
      finer(s"theSession.setCursor($row, $col)->$newcursor ($selection)")
    }
    if (selection ne NoSelection) selection = Span(selection.cursor, selection.mark, false) //selection.tentative=false
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

  /**
   * Distance of the first non-space character from the start
   * of the current line.
   */
  def currentIndent: Int = {
    val (row, col) = document.positionToCoordinates(cursor)
    var startLine  = document.coordinatesToPosition(row, 0)
    var indent     = 0
    while (indent<col && document.character(startLine+indent)==' ') indent += 1
    indent
  }

  /**
   * Distance of the first non-space character to the right of the cursor
   */
  def currentLead: Int = {
    var lead     = cursor
    while (lead<document.textLength && document.character(lead)==' ') lead += 1
    lead-cursor
  }

  /**
   * Distance of the first non-space character to the left of the cursor
   */
  def currentTrail: Int = {
    var trail     = cursor-1
    while (trail>=0 && document.character(trail)==' ') trail -= 1
    cursor-trail-1
  }


  def nextTabStop: Int = {
    var (row, col) = document.positionToCoordinates(cursor)
    var indent     = col + 1
    while (indent%8!=0) indent += 1
    indent-col
  }

  /** Set the mark and change the selection. */
  def setMark(newPosition: Int, tentative: Boolean=false): Unit = {
    if (logging) finer(s"theSession.setMark($newPosition)")
    selectUntil(newPosition, tentative)
  }

  /**
   * Change the current selection so that it spans the current `cursor`
   * position and the given `mark` position.
   */
  def selectUntil(mark: Int, tentative: Boolean = false): Unit = {
    if (logging) finer(s"selectUntil $cursor -> $mark")
    selection = Span(cursor, mark, tentative)
  }

  ///////////////////////////////////////////////////////////////////////////////////
  /////////////////////////// Editing Command implementations ///////////////////////
  //////////////////////////////////////////////////////////////////////////////////

  var typeOverSelection: Boolean = Personalised.Settings.typeOverSelection
  var autoIndenting:     Boolean = Personalised.Settings.autoIndenting
  var clickSelects:      Boolean = Personalised.Settings.clickSelects
  var insertionSelects:  Boolean = Personalised.Settings.insertionSelects

  /** Remove the mark
   *  (OPTIONAL PRACTICAL: cut the selection if in typeover mode)
   */
  def deSelect(): Unit = selection = NoSelection

  def typeOverMode: Boolean = typeOverSelection

  def insert(ch: Char): Unit = {
    deSelect()
    cursor = document.insert(cursor, ch)
  }

  /** Insert a newline followed by an indentation, and ensure that
   * a screen refresh is triggered. A little inelegantly this
   * is done by setting the one-purpose flag, `forceRefresh`.
   * TODO: revise the refresh-triggering machinery; perhaps by counting changes since the last refresh
   */
  def insertNewlineAndIndentBy(indent: Int): Unit = {
    insert('\n')
    for { i<- 0 until indent } insert(' ')
    forceRefresh = true
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

  /** Exchange the two characters just before the cursor. */
  def flip(): Unit = {
    if (cursor>=2) {
      val text = document.getString(cursor-2, cursor)
      deleteFor(-2)
      insert(text.reverse)
    }
  }

  def clear(): Unit = {
    document.delete(0, document.textLength)
    cursor = 0
    selection = NoSelection
    forceRefresh = true
    insert("\n") // was commented out
    notifyHandlers()
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

  /**
   *  Paste the system clipboard into the text at the
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
    selection = Span(cursor, oldCursor, true)
    theText
  }

  /**
   *  Paste `theClip` and select it, with mark to the left.
   *  Cut and return the previously-selected text.
   *
   *  @param requireSelection true when an actual selection is required
   */
  def exch(theClip: String, requireSelection: Boolean): String = {
    if (requireSelection && hasNoSelection) "" else {
      val text = selectionText()
      SystemClipboard.set(text)
      recordCut(Cut(text, selection.extent, selection.cursor, document.generation))
      document.delete(selection.left, selection.right-selection.left)
      if (selection.extent<0) cursor += selection.extent
      val oldCursor = cursor
      insert(theClip)
      selection = Span(cursor, oldCursor, true)
      text
    }
  }

  def toEnd(): Unit = { cursor = document.textLength }

  def toHome(): Unit = { cursor = 0 }

  def selectAll(): Unit = {
    selection = Span(0, document.textLength, true)
    cursor    = 0
  }

  /**
   *  Definitions related to bracket-matching. This will eventually be generalized and
   *  become configurable by end users. But there are some corner cases, leading to
   *  ambiguities. These made it necessary to manage xml/latex blocks by using special cases.
   */
  object Bracketing {
    import sufrin.regex.Brackets
    val begin       = Brackets("""\\begin""", """\\end{[^}]+}""")
    val brace       = Brackets("""{""", """}""")
    val par         = Brackets("""\(""", """\)""")
    val bra         = Brackets("""\[""", """\]""")
    val slashpar    = Brackets("""\\\(""", """\\\)""")
    val slashbra    = Brackets("""\\\[""", """\\\]""")
    val slashbrace  = Brackets("""\\{""", """\\}""")


    // XML block and single tag patterns -- determined by experiment
    val xmlblock    = Brackets("""<([A-Za-z0-9:]+)(\s+([A-Za-z0-9:]+\s*=\s*"[^"]*"))*>""", """</[A-Za-z0-9]+>""")
    val xmlsingle   = Brackets("""<([A-Za-z0-9:]+)(\s+([A-Za-z0-9:]+\s*=\s*"[^"]*"))*""", """/>""")
    val xmlcomment  = Brackets("""<!--""", """-->""")

    val xmlSpecs       = List[Specification](xmlblock, xmlsingle, xmlcomment)
    val compositeSpecs = List[Specification](begin, brace, par, bra, slashpar, slashbra, slashbrace)++xmlSpecs
    val compositeBras: List[Tree[Char]] = compositeSpecs.map(_.bra.tree)
    val compositeKets: List[Tree[Char]] = compositeSpecs.map(_.ket.tree)
    //  characters treated specially by `selectMatchingUp`
    val compositeKetSpecials: List[Tree[Char]] = "}])".toList.map(Literal(_)) ++ xmlSpecs.map(_.ket.tree)


    val lefts, rights: collection.mutable.HashMap[Char,Brackets.Specification] = collection.mutable.HashMap[Char,Brackets.Specification]()
    locally {
      val simplePairs = List (
        "«"    -> "»",
        "⁅"    -> "⁆",
        "/\\*" -> "\\*/",
        "``"   -> "''",  // For ascii latex double brackets
        // Matched pairs (they happen to be adjacently unicoded)
        "\u2018"    -> "\u2019",
        "\u201c"    -> "\u201d",
        "\u2039"    -> "\u203a",
        "\u201c"    -> "\u201d",
        "⟦" -> "⟧", // '\u27e7'
        "⟨" -> "⟩", // '\u27e9'
        "⟪" -> "⟫", // '\u27eb'
        "⟬" -> "⟭", // '\u27ed'
        "⟮" -> "⟯", // '\u27ef'
        "⸨" -> "⸩" // '\u2e29'
      )
      for { (l, r) <- simplePairs } {
        val spec = Brackets(l, r)
        lefts.addOne((l.head, spec))
        rights.addOne((r.last, spec))
      }
    }

    val anyBra = {
      val trees: Seq[Tree[Char]] = compositeBras ++ lefts.values.map(_.bra.tree)
      val tree: Tree[Char] = trees . reduce (Alt(_,_))
      new Regex(tree, false, false, -1)
    }

    val anyKet = {
      val trees: Seq[Tree[Char]] =  compositeKetSpecials ++ rights.values.map(_.ket.tree)
      val tree: Tree[Char] = trees . reduce (Alt(_,_))
      val res = new Regex(tree, false, false, -1)
      res
    }

    /**
     *  If the `ket` of this bracketing specification appears at
     *  the left of the cursor, then set the mark to the starting
     *  `bra` of the properly-nested (wrt bra/ket) sequence to
     *  the left.
     */
    def tryMatchUp(spec: Brackets.Specification): Boolean = {
      if (spec.ket.suffixes(document.characters, 0, cursor).isEmpty) false
      else {
        if (logging) fine(s"tryMatchup: $spec suffixes")
        spec.matchBackward(document.characters, 0, cursor) match {
          case None =>
            if (logging) fine(s"tryMatchup: $spec fails to match backward")
            false
          case Some(start) =>
            if (logging) fine(s"tryMatchup: $spec matches backward at $start")
            setMark(start, tentative=true)
            true
        }
      }
    }

    /**
     *  If the `bra` of this bracketing specification appears at
     *  the right of the cursor, then set the mark to the closing
     *  `ket` of the properly-nested (wrt bra/ket) sequence to
     *  the right.
     */
    def tryMatchDown(spec: Brackets.Specification): Boolean = {
      if (logging) finest(s"tryMatchDown $spec")
      if (spec.bra.prefixes(document.characters, cursor, document.characters.length).isEmpty) false else
      spec.matchForward(document.characters, cursor, document.characters.length) match {
        case None      => false
        case Some(end) => setMark(end, tentative=true); true
      }
    }
  }

  // TODO: Generalize by removing specific trigger-characters
  /**
   * If the character to the left of the cursor could end
   * a properly-bracketed construction, then select from the start
   * of that bracketed construction (if it exists), and yield `true`.
   */
  def selectMatchingUp(): Boolean   = {
    import Bracketing._
    if (cursor==0) false else {
      // A few kets share suffixes: we resolve the ambiguity with a sledgehammer (attempting the upwards matches sequentially)
      // (See also  `compositeKetSpecials`)
      document.character(cursor-1) match {
        case '}' => tryMatchUp(begin)      || tryMatchUp(slashbrace) || tryMatchUp(brace)     // not particularly inefficient (see tryMatchUp)
        case '>' => tryMatchUp(xmlcomment) || tryMatchUp(xmlblock) || tryMatchUp(xmlsingle)   // not particularly inefficient (see tryMatchUp)
        case ')' => tryMatchUp(slashpar)   || tryMatchUp(par)
        case ']' => tryMatchUp(slashbra)   || tryMatchUp(bra)
        case ch  => rights.get(ch) match {
          case Some(spec) => tryMatchUp(spec)
          case None       => false
        }
      }
    }
  }

  // TODO: Generalize by removing specific trigger-characters
  /**
   * If the character to the right of the cursor could begin
   * a properly-bracketed construction, then select to the end
   * of that bracketed construction (if it exists), and
   * yield `true`.
   */
  def selectMatchingDown(): Boolean = {
    import Bracketing._
    if (cursor>=document.textLength) false else {
      document.character(cursor) match {
        case '\\' => tryMatchDown(begin)
        case '{'  => tryMatchDown(brace)
        case '<'  => tryMatchDown(xmlcomment) || tryMatchDown(xmlblock) || tryMatchDown(xmlsingle)
        case '('  => tryMatchDown(par)
        case '['  => tryMatchDown(bra)
        case ch  => lefts.get(ch) match {
          case Some(spec) => tryMatchDown(spec)
          case None       => false
        }
      }
    }
  }

  def anyBraUpwards(): Boolean = {
    if (cursor==0) false else {
      val left = Bracketing.anyBra.findSuffix(document.characters, 0, cursor)
      left match {
        case None => false
        case Some(theMatch) =>
          val oldCursor = cursor
          cursor = theMatch.start
          if (selectMatchingDown()) true else {
            cursor = oldCursor
            false
          }
      }
    }
  }

  def anyKetDownwards(): Boolean = {
    val length = document.characters.length()
    if (cursor>=length) false else {
      val right = Bracketing.anyKet.findPrefix(document.characters, cursor, length)
      right match {
        case None => false
        case Some(theMatch) =>
          val oldCursor = cursor
          cursor = theMatch.end
          if (logging) fine(s"$oldCursor=>$cursor '${document.character(cursor)}' ($theMatch)")
          if (selectMatchingUp()) true else {
            cursor = oldCursor
            false
          }
      }
    }
  }

  /**
   *  Patterns matching the left and right boundaries of
   *  various granularities of text lump.
   */
  object Boundaries {
    import sufrin.regex.Regex
    val leftWord  : Regex   = Regex("""\W\w""")
    val rightWord : Regex   = Regex("""\w\W""")
    val leftLine  : Regex   = Regex("""(\n|^)[^\n]*""")
    val rightLine : Regex   = Regex.literal("\n") // TODO: why doesn't Regex("""\n""")  match at (**)
    val leftPara  : Regex   = Regex("(\n|^)\\s*\n")
    val rightPara : Regex   = Regex("\n\\s*(\n|$)")
  }

  def selectParagraph(): Unit = selectChunkMatching(cursor, Boundaries.leftPara, Boundaries.rightPara, 1, 1)

  def atRightOf(left: Regex): Boolean = left.suffixes(document.characters, 0, cursor).nonEmpty

  def atLeftOf(right: Regex): Boolean = right.prefixes(document.characters, cursor, document.characters.length).nonEmpty

  def selectChunkMatching(startingCursor: Int, left: Regex, right: Regex, adjl: Int, adjr: Int): Unit =
    left.findSuffix(document.characters, 0, startingCursor) match {
      case None =>
      case Some(leftp) =>
        if (logging) finest(s"leftp=${(leftp.start, leftp.end)} (${leftp})")
        right.findPrefix(document.characters, leftp.end, document.characters.length) match {
          case None =>
            if (logging) finest(s"NO RIGHT MATCH FOR $right ${leftp.end}..${document.characters.length}") // (**)
          case Some(rightp) =>
            if (logging) finest(s"rightp=${(rightp.start, rightp.end)} ($rightp)")
            val (start, end) = (if (leftp.start == 0) leftp.start else leftp.start + adjl, rightp.end - adjr)
            if (logging) finest(s"(start, end)=${(start, end)}")
            if (startingCursor - start > end - startingCursor) {
              cursor = end
              setMark(start)
            }
            else {
              cursor = start
              setMark(end)
            }
        }
    }

  /** `2 <= clicks <= 5` */
  def selectChunk(row: Int, col: Int, clicks: Int): Unit = {
    val startingCursor = document.coordinatesToPosition(row, col)

    import Boundaries._
    clicks match {
      case 2 => selectChunkMatching(startingCursor, leftWord, rightWord, 1, 1) // word
      case 3 => selectChunkMatching(startingCursor, leftLine, rightLine, 1, 0) // line
      case 4 => selectChunkMatching(startingCursor, leftPara, rightPara, 1, 1) // para
      case 5 => // select the closest balanced \begin/\end block ending below the click position
                // If the button was clicked nearer to the start of the block than to the end
                // then the cursor and mark are placed at start/end of the selection
                Bracketing.begin.ket.findPrefix(document.characters, startingCursor) match {
                  case None =>
                  case Some(ket) =>
                    Bracketing.begin.matchBackward(document.characters, 0, ket.end) match {
                      case None =>
                      case Some(braPos) =>
                        val (start, end) = (braPos, ket.end)
                        if (startingCursor - start > end - startingCursor) {
                          cursor = end
                          setMark(start)
                        }
                        else {
                          cursor = start
                          setMark(end)
                        }
                    }
                }
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

  val feedback: Notifier[(String,String)] = new Notifier[(String, String)]

  /**
   * Cache for most recent regex: avoids recompilation on
   * repeated find/repl.
   */
  object RegexCache {
    import sufrin.regex._
    var lastExpr: String  = _
    var lastLit:  Boolean = _
    var lastRegex: Regex  = _
    var cacheValid = false

    def apply(expr: String, lit: Boolean): Regex = {
      if (!cacheValid || expr != lastExpr || lit != lastLit) {
        lastExpr   = expr
        lastLit    = lit
        lastRegex  = if (lit) Regex.literal(lastExpr) else Regex(lastExpr)
        cacheValid = true
      }
      lastRegex
    }
  }

  /**
   *  TODO: `find` for a poorly-constructed regex may take forever to terminate.
   *        Regex compilation should filter the antipatterns: but even dealing
   *        with the more obvious ones slows down the compiler, and
   *        loop detection in the abstract machine is likely to make it slower.
   *        It is possible to limit the resources used during a find; and
   *        to interrupt if the limit is exceeded. The simplest resource
   *        to limit is cycles of the abstract machine. Limiting ''time'' will
   *        be more complicated. DONE.
   */
  def find(thePattern: String, backwards: Boolean, asRegex: Boolean): Boolean = {
    try {
      val regex = RegexCache(thePattern, !asRegex)
      if (backwards) {
        val lastMatch = regex.findSuffix(document.characters, 0, cursor, Utils.stepLimit)
        lastMatch match {
          case None =>
            feedback.notify("Not found upwards", s"$thePattern")
            false
          case Some(instance) =>
            if (Utils.showSteps) feedback.notify("Found", s"#${instance.steps}/${Utils.stepLimit}")
            cursor = instance.start
            setMark(instance.end)
            true
        }
      } else {
        val lastMatch = regex.findPrefix(document.characters, cursor, document.characters.length, Utils.stepLimit)
        lastMatch match {
          case None =>
            feedback.notify("Not found", s"$thePattern")
            false
          case Some(instance) =>
            if (Utils.showSteps) feedback.notify("Found", s"#${instance.steps}/${Utils.stepLimit}")
            //TODO: remove min hack designed to avoid embarrassment if $ matched
            cursor = instance.end min (document.characters.length-1)
            setMark(instance.start)
            true
        }
      }
    } catch {
      case exn: sufrin.regex.machine.State.StepLimitExceeded =>
        warnings.notify("Find", s"Pattern:\n  $thePattern\n exceeded the findsteps limit (${Utils.stepLimit})\n ${exn.getMessage}")
        false
      case exn: sufrin.regex.syntax.lexer.SyntaxError =>
        warnings.notify("Find", s"Pattern:\n  $thePattern\n is not well-formed\n ${exn.getMessage}")
        false
      case exn: java.lang.RuntimeException =>
        //exn.printStackTrace()
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
  def replace(thePattern: String, theReplacement: String, backwards: Boolean, asRegex: Boolean): Option[String] = try {
    val lastMatch = RegexCache(thePattern, !asRegex).matches(selectionText())
    lastMatch match {
      case None =>
        warnings.notify("Replace", s"Pattern:\n  $thePattern\n is not matched by the selection.")
        None
      case Some(instance) =>
        val repl = if (asRegex) instance.substitute(theReplacement) else theReplacement
        exch(repl, true)
        if (backwards) {
          val c = cursor;
          cursor -= repl.length;
          setMark(c)
        }
        Some(instance.matched)
    }

  } catch {
    case exn: java.lang.RuntimeException =>
      warnings.notify("Replace", s"Pattern:\n  $thePattern\n is not well-formed\n ${exn.getMessage}")
      None
  }

  def replaceAllInSelection(thePattern: String, theReplacement: String, asRegex: Boolean): Option[String] =
    try {
      val regex = RegexCache(thePattern, !asRegex)
      val selected = selectionText()
      val (count, repl) = regex.substituteAll(selected, theReplacement, !asRegex)
      feedback.notify("Replace All", s"$count replacements")
      if (count>0) { exch(repl, true); Some(selected) } else None
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
   * Set the cursor and mark whilst noting the dragging origin
   * if a drag has just started. Extend the selection only if
   * there already is a selection and the cursor has been
   * dragged over at least one character. This condition
   * avoids turning a tentative to a definite selection by
   * an accidental nudge when the mouse is down.
   * Notify observers of changes in the selection/cursor.
   */
  def dragCursor(row: Int, col: Int): Unit = {
    if (logging)
      finer(s"DragCursor($row, $col) with selection=$selection")
    val newcursor = document.coordinatesToPosition(row, col)
    if (draggingFrom.isEmpty) draggingFrom = Some(cursor)
    val moved = draggingFrom.get - cursor
    cursor=newcursor
    // The following condition requires at least one
    // character of movement before we change the selection.
    if (moved!=0 && (selection ne NoSelection)) selectUntil(selection.mark)
  }

  //////////////////////////////////////////
  //
  //  Name, path, working directory
  //
  /////////////////////////////////////////


  /**
   * Displayable path: sometimes relative to the HOME directory, sometimes with ellipsis
   */
  var displayPath: String = Utils.displayablePath(this._path)

  def path_=(_newPath: String): Unit = {
    val oldPath = Utils.toPath(_path)
    val newPath = Utils.toPath(_newPath)
    // tell everything that might care: in particular the session manager
    if (oldPath != newPath) pathChange.notify(oldPath, newPath)
    this._path = newPath.toString
    displayPath = Utils.displayablePath(this._path)
  }

  def path: String = _path

  val pathChange: Notifier[(Path, Path)] = new Notifier("pathChange")

  def parentPath: Path = Utils.toPath(path).getParent

  // Current working directory
  private var _CWD: Path = parentPath // Utils.homePath

  def CWD_=(path: Path) = {
    _CWD = path
  }

  def CWD: Path = _CWD

  // Current tex master
  private var _TEX: Path = null

  def TEX_=(path: Path) = {
    _TEX = path
  }

  def TEX: Path = if (_TEX == null) Utils.toPath(path) else _TEX

} // EditSession

object EditSession extends Logging.Loggable


/**
 * Representation of the state of a selection in an editing session.
 * An `tentative` selection is one made to indicate a certain
 * region of text that should probably not be cut automatically
 * in insert-cuts-selection mode. It can still be cut deliberately
 * by the usual commands.
 */
case class Span(cursor: Int, mark: Int, var tentative: Boolean=false) {
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

