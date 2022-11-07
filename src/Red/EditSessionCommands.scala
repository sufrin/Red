package Red

import Commands.{Command, StateChange}
import Red.FilterUtilities.inputStreamOf
import RedScript.Language.{SExps, Str, Variable}

import java.nio.file.Path
import scala.sys.process.{Process, ProcessLogger}

/**
 *   `Command`s derived from `EditSession` methods.
 *
 *   Commands are constants, except for those that
 *   are parameterised by characters (to insert), strings
 *   (for find/replace) or document positions.
 *
 *   TODO:  1. abstract patterns where possible
 *          2. (?) dictionary for constant session commands and maybe for
 *          mouse-invoked session commands.
 *          3. Build the dictionary by annotations?
 *
 */
object EditSessionCommands extends Logging.Loggable {

  type SessionCommand    = Command[EditSession]
  type StateChangeOption = Option[StateChange]

  /**
   *  Insert a newline. If the session is in `autoIndenting` mode
   *  insert enough spaces to align the left margin to the indentation of
   *  the current line; remove any leading spaces to the right of the cursor
   *  and trailing spaces to the left of the cursor.
   */
  val autoIndentNL: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = Some {
      val indent = if (session.autoIndenting) session.currentIndent else 0
      val leading = if (session.autoIndenting) session.currentLead else 0
      val trailing = if (session.autoIndenting) session.currentTrail else 0
      val oldCursor = session.cursor
      val oldSelection = session.selection
      new StateChange {
        def undo(): Unit = {
          session.deleteFor(-(indent+1), record=false)
          for { i<-0 until leading+trailing } session.insert(' ')
          session.selection = oldSelection
          session.cursor = oldCursor
        }
        def redo(): Unit = {
            session.deleteFor(-trailing, record=false)
            session.deleteFor(leading, record=false)
            session.insertNewlineAndIndentBy(indent)
        }
        override val kind: String = "InsEol"
        locally { redo() }
      }
    }
  }

  // TODO: make these a user preference
  private val indentBy = "    "
  private val undentBy = "    "

  /**
   *  Prefix each line of the current selection with
   *  `indentBy`.
   */
  val indentSelection: SessionCommand = indentSelectionBy(indentBy)

  def indentSelectionBy(indentBy: String): SessionCommand = new Filter {
    override def adjustNL: Boolean = true
    override val kind: String = "indentSel"
    protected override def transform(input: String, cwd: Path): Option[String] = {
      val lines = input.split('\n')
      val result= new StringBuilder()
      for { line <- lines } { result.addAll(indentBy); result.addAll(line); result.addOne('\n') }
      Some(result.toString())
    }
  }

  /**
   *  Remove the prefix `undentBy` from each line of the
   *  current selection.
   */
  val undentSelection: SessionCommand = undentSelectionBy(undentBy)

  def undentSelectionBy(undentBy: String): SessionCommand = new Filter {
    override def adjustNL: Boolean = true
    override val kind: String = "undentSel"
    protected override def transform(input: String, cwd: Path): Option[String] = {
      val lines = input.split('\n')
      val result= new StringBuilder()
      for { line <- lines } {
        result.addAll(if (line.startsWith(undentBy)) line.substring(undentBy.length) else line)
        result.addOne('\n')
      }
      Some(result.toString())
    }
  }

  val autoIndentSelection: SessionCommand =
      Command.guarded( _.hasSelection, indentSelection )

  val autoTab: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = Some {
      val indent = session.nextTabStop
      new StateChange {
        def undo(): Unit = {
          session.deleteFor(-indent, record=false)
        }

        def redo(): Unit =
          for { i<-0 until indent } session.insert(' ')

        override val kind: String = "InsTab"
        locally { redo() }
      }
    }
  }

  def formatter(arg: String, program: String = "fmt", unchecked: List[String] = List()): SessionCommand = {
    def numOrSwitch(arg: String): Boolean = {
      arg.startsWith("-") || arg.matches("[0-9]+") || unchecked.contains(arg)
    }

    def checkArgs(args: Seq[String]): Option[String] =
      if (args.tail forall numOrSwitch)
        None
      else
        Some("formatter arguments in \u24b6 must be numbers or -switches")

    pipeThrough(s"$program $arg", checkArgs(_))
  }

  def pipeThrough(arg: String, errorCheck: Seq[String] => Option[String] = { args => None }, replaceSelection: Boolean = true): SessionCommand = new Filter {
    protected override def transform(input: String, cwd: Path): Option[String] = {
      val args = FilterUtilities.parseArguments(arg)
      errorCheck(args) match {
        case None =>
          val cmd = Process(args, cwd.toFile)
          Some(if (replaceSelection) Filter.runProcess(cmd, input) else Filter.runProcess(cmd, input) ++ input)
        case Some(error) =>
          Filter.warnings.notify(arg, error)
          None
      }
    }
  }

  def pipeThroughScript(functionName: String, path: String, argLine: String, findLine: String, replLine: String, replaceSelection: Boolean = true): SessionCommand = new Filter {
    val evaluator = Personalised.Bindings.RedScriptEvaluator
    val global = evaluator.global

    protected override def transform(input: String, cwd: Path): Option[String] = {
      try {
        Some((if (replaceSelection) "" else input) ++ evaluator.run(SExps(List(Variable(functionName), Str(path), Str(argLine), Str(findLine), Str(replLine), Str(input)))).toPlainString)
      } catch {
        case exn => Some(exn.toString)
      }
    }
  }

  def unhandledInput(key: UserInput): SessionCommand = new Filter {
    val evaluator = Personalised.Bindings.RedScriptEvaluator
    val global = evaluator.global

    protected override def transform(input: String, cwd: Path): Option[String] = {
      try {
        val expr = Personalised.Bindings.RedScriptEvaluator.UserInput(key)
        evaluator.run(evaluator.run(SExps(List(Variable("unhandledInput"), expr)))) match {
          case Str(s) => Some(s+"\n")
          case _      => None
        }
      } catch {
        case exn => Some(exn.toString)
      }
    }
  }

  /**
   *   A utility that whose result is a `SessionCommand` that is equivalent to `command`
   *   if the session is in type-over-selection mode and there is a selection.
   *   Otherwise the result simply succeeds having done nothing.
   */
  def ifTypeOver(command: SessionCommand): SessionCommand =
    Command.when((s: EditSession) => s.typeOverMode && s.hasSelection && !s.selection.indicative, command)

  /**  A utility command that always succeeds after notifying the
   *   session's handlers that something in the session (may have) changed.
   *   When sequentially composing a session command from two or more others,
   *   it's usually necessary to synchronize the context (in particular
   *   the document display) with the session after all but the last session
   *   command.
   */
  val notifyNow: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      session.notifyHandlers()
      Some(Command.undoNothing)
    }
  }

  /**
   * An insertion from the keyboard that cuts the selection first if
   * the session is in type-over-selection mode, and the selection
   * isn't indicative.
   */
  def insertCommand(ch: Char): SessionCommand = ifTypeOver(cut &&& notifyNow) &&& insert(ch)

  /**
   *  An `InsChange` is either the representation of a `StateChange` for a single character insertion,
   *  or the representation of a `StateChange` for a sequence of single character insertions.
   *
   *  The implementation makes the representation of sequences of successive insertions
   *  more concise (by a constant factor) than the composition of the more obvious
   *  conventional `StateChange` representations.
   *
   *  To be more precise: the cost of maintaining the `StateChange` for the insertion, without
   *  interrupting, of a line of length `N (>1)` by composing conventional `StateChange`s is
   *  {{{N*SIZE(StateChange)}}}
   *  whereas the cost of using `InsChange` is
   *  {{{SIZE(InsChange)+SIZE(StringBuilder(N))}}}
   *
   *  Merging `this: InsChange` with `next: InsChange`  ''always'' yields `this`, having
   *  changed `this.chars` (if necessary) to be the `StringBuilder` for which `undo` and
   *  `redo` have the appropriate effects.
   *
   */
  class InsChange(session: EditSession, var chars: Either[Char, StringBuilder]) extends StateChange {
    override val kind: String = if (isEol(chars)) "InsEol" else "Ins" // break insertion merges

    def undo(): Unit = session.deleteFor(-length)

    @inline def length: Int = chars match {
      case Left(_) => 1
      case Right(builder) => builder.length
    }

    def redo(): Unit = chars match {
      case Left(ch) => session.insert(ch)
      case Right(builder) => session.insert(builder.toString())
    }

    /**
     * '''Precondition:'''
     *   {{{ next.isInstanceOf[InsChange] => next.chars.isLeft }}}
     */
    override def merge(next: StateChange): StateChangeOption = {
      next match {
        case next: InsChange =>
          assert(next.chars.isLeft, "Attempt to merge a non-unit InsChange with an insChange")
          chars match {
            case Left(ch) =>
              val builder: StringBuilder = new StringBuilder(ch)
              builder.append(next.chars.left)
              chars = Right(builder)

            case Right(builder) =>
              builder.append(next.chars.left)
          }
          println(s"InsChange($chars)")
          Some(this)
        case other => None
      }
    }

    @inline def isEol(e: Either[Char, Any]): Boolean = e match {
      case Left('\n') => true;
      case _ => false
    }
  }

  def insert(ch: Char): SessionCommand = {
    if (ch == '\n') autoIndentNL else new SessionCommand {
      def DO(session: EditSession): StateChangeOption = {
        session.insert(ch)
        Some(new InsChange(session, Left(ch)))
      }
    }
  }

  val delete: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption =
      if (session.cursor == 0) None
      else
        Some {
          val ch = session.document.character(session.cursor - 1)
          session.delete()
          new StateChange {
            def undo(): Unit = session.insert(ch)

            def redo(): Unit = session.delete()

            // deletions are chunked by the lineful
            override val kind: String = if (ch == '\n') "DelEol" else "Del"
          }
        }
  }

  val flip: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption =
      if (session.cursor < 2) None
      else
        Some {
          session.flip()
          new StateChange {
            def undo(): Unit = session.flip()

            def redo(): Unit = session.flip()
          }
        }
  }

  val nextLine: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldCursor = session.cursor
      if (session.nextLine()) Some {
        new StateChange {
          def undo(): Unit = session.cursor = oldCursor

          def redo(): Unit = session.nextLine()

          override val kind: String = "->"
        }
      }
      else None
    }
  }

  val prevLine: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldCursor = session.cursor
      if (session.prevLine()) Some {
        new StateChange {
          def undo(): Unit = session.cursor = oldCursor

          def redo(): Unit = session.prevLine()

          override val kind: String = "<-"
        }
      }
      else None
    }
  }

  val prevChar: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      if (session.prevChar()) Some {
        new StateChange {
          def undo(): Unit = session.nextChar()

          def redo(): Unit = session.prevChar()

          override val kind: String = "<-"
        }
      }
      else None
    }
  }

  val nextChar: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      if (session.nextChar()) Some {
        new StateChange {
          def undo(): Unit = session.prevChar()

          def redo(): Unit = session.nextChar()

          override val kind: String = "->"
        }
      }
      else None
    }
  }

  val selectParagraph: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldCursor = session.cursor
      val oldSelection = session.selection
      session.selectParagraph()
      Some(new StateChange {
        def undo(): Unit = {
          session.selection = oldSelection; session.cursor = oldCursor
        }

        def redo(): Unit = session.selectParagraph()

        override val kind: String = "->"
      })
    }
  }

  def setCursor(row: Int, col: Int): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldCursor = session.cursor
      session.setCursor(row, col)
      Some {
        new StateChange {
          def undo(): Unit = session.cursor = oldCursor

          def redo(): Unit = session.setCursor(row, col)
        }
      }
    }
  }

  /** Employed in implementing cursor-drag selections.
    *  The undo must reset the selection
    */
  def setCursorAndMark(row: Int, col: Int): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldCursor = session.cursor
      val oldSelection = session.selection
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection; session.cursor = oldCursor
          }

          def redo(): Unit = {
            session.setCursorAndMark(row, col)
          }

          locally {
            redo()
          }
        }
      }
    }
  }

  /**
   * Dragging the cursor is unusual.
   * There is no need for a history item, because a drag will always be
   * preceded by a `setCursorAndMark`, and followed by a `mouseUp`
   * whose undo method will suffice to undo the whole drag sequence, and
   * whose redo method simply restores the selection as it was at the point the mouse
   * was released.
   *
   * '''TL;DR '''
   * In any case, having a history item per drag event would be
   * costly in terms of space and bandwidth: even if the drags were (as they
   * should be) merged.
   */
  def dragCursor(row: Int, col: Int): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      session.dragCursor(row, col)
      None
    }
  }

  /**
   *  The end of a sequence of cursor drags is recorded
   *  in the history as if it were a single selection
   *  event.
   *
   */
  val mouseUp: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      if (session.draggingFrom.isEmpty)
        None
      else Some(new StateChange {
        val oldCursor = session.draggingFrom.get
        val oldSelection = session.selection
        session.stopDragging

        def undo(): Unit = {
          session.cursor = oldCursor; session.selection = NoSelection
        }

        def redo(): Unit = {
          session.cursor = oldSelection.cursor; session.setMark(oldSelection.mark)
        }
      })
    }
  }

  def setMark(row: Int, col: Int): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      session.setMark(row, col)
      Some {
        new StateChange {
          def undo(): Unit = session.selection = oldSelection

          def redo(): Unit = session.setMark(row, col)
        }
      }
    }
  }

  def selectMatching(select: EditSession => Boolean): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      if (!select(session)) None else
        Some {
          new StateChange {
            def undo(): Unit = session.selection = oldSelection

            def redo(): Unit = select(session)
          }
        }
    }
  }

  val selectMatchingUp: SessionCommand   = selectMatching(_.selectMatchingUp())
  val selectMatchingDown: SessionCommand = selectMatching(_.selectMatchingDown())
  val selectMatching: SessionCommand     = (selectMatchingUp ||| selectMatchingDown ||| Command.doNothing)

  val copy: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = if (session.hasNoSelection) None
    else {
      val oldSelection = session.selection
      val oldClip = SystemClipboard.getOrElse("")
      session.copy()
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            SystemClipboard.set(oldClip)
          }

          def redo(): Unit = session.copy()
        }
      }
    }
  }

  val cut: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = if (session.hasNoSelection) None
    else {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      val oldClip = SystemClipboard.getOrElse("")
      val oldSelected = session.selectionText()
      Some {
        new StateChange {
          def undo(): Unit = {
            SystemClipboard.set(oldClip)
            // Should restore a selection with the correct polarity
            session.cursor = oldSelection.left // reposition the session
            session.insert(oldSelected) // insert the old selected text
            session.cursor = oldSelection.cursor
            session.selection = Span(session.cursor, oldSelection.mark)
          }

          def redo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
            session.cut()
          }

          locally {
            redo()
          }
        }
      }
    }
  }

  val paste: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      val oldClip = SystemClipboard.getOrElse("")
      session.paste(oldClip)
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
            session.delete(oldClip.length)
          }

          def redo(): Unit = session.paste(oldClip)
        }
      }
    }
  }

  val toHome: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      session.cursor = 0
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
          }

          def redo(): Unit = session.cursor = 0
        }
      }
    }
  }

  val toEnd: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      session.cursor = session.document.textLength
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
          }

          def redo(): Unit = session.document.textLength
        }
      }
    }
  }

  val selectAll: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      session.selectAll()
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
          }

          def redo(): Unit = session.selectAll()
        }
      }
    }
  }

  /** Select the surrounding chunk of text depending on `clicks`: word/line/para/latex-block  */
  def selectChunk(row: Int, col: Int, clicks: Int): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      session.selectChunk(row, col, clicks)
      Some {
        val newCursor = session.cursor
        val newSelection = session.selection
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor = oldCursor
          }

          def redo(): Unit = {
            session.cursor = newCursor
            session.setMark(newSelection.mark)
          }

          override val kind: String = "selectChunk"
        }
      }
    }
  }


  val clearAll: SessionCommand = selectAll &&& cut

  val exchangeCut: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldClip = SystemClipboard.getOrElse("")
      val oldSelection = session.selection // get the polarity right on undo
      val oldSelected = session.exch(oldClip, true)
      Some {
        new StateChange {
          def undo(): Unit = {
            session.exch(oldSelected, true); session.selection = oldSelection
          }

          def redo(): Unit = session.exch(oldClip, true)
        }
      }
    }
  }

  def find(thePattern: String, backwards: Boolean, asRegex: Boolean)(onFind: => Unit): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor = session.cursor
      if (session.find(thePattern, backwards, asRegex)) {
        onFind
        Some(new StateChange {
          def undo(): Unit = {
            session.cursor = oldCursor; session.selection = oldSelection
          }

          def redo(): Unit = session.find(thePattern, backwards, asRegex)
        })
      } else None
    }
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean, asRegex: Boolean): SessionCommand =
    new SessionCommand {
      def DO(session: EditSession): StateChangeOption = {
        val replaced = session.replace(thePattern, theReplacement, backwards, asRegex)
        if (replaced.isDefined) Some(new StateChange {
          def undo(): Unit = {
            session.exch(replaced.get, true)
          }

          def redo(): Unit = session.replace(thePattern, theReplacement, backwards, asRegex)
        }) else None
      }
    }

  def replaceAllInSelection(thePattern: String, theReplacement: String, asRegex: Boolean): SessionCommand =
    new SessionCommand {
      def DO(session: EditSession): StateChangeOption = {
        val replaced = session.replaceAllInSelection(thePattern, theReplacement, asRegex)
        if (replaced.isDefined) Some(new StateChange {
          def undo(): Unit = {
            session.exch(replaced.get, true)
          }

          def redo(): Unit = session.replaceAllInSelection(thePattern, theReplacement, asRegex)
        }) else None
      }
    }

  val lowerCaseFilter: SessionCommand = new Filter {
    protected override def transform(input: String, cwd: Path): Option[String] = Some(input.toLowerCase())
  }

  def latexBlock(block: String): SessionCommand = new Filter {
    override def adjustNL: Boolean = false

    protected override def transform(input: String, cwd: Path): Option[String] = {
      val nl = if (input != "" && input.last != '\n') "\n" else ""
      Some(s"""\\begin{$block}\n$input$nl\\end{${block}}""")
    }
  }

  def latexInsert(text: String): SessionCommand = new Filter {
    protected override def transform(input: String, cwd: Path): Option[String] = {
      Some(s"$text\n$input")
    }
  }

  // TODO: use sufrin.regex kit
  val latexUnblock: SessionCommand = new Filter {
    override def adjustNL: Boolean = false

    protected override def transform(input: String, cwd: Path): Option[String] = {
      import scala.util.matching.Regex
      val pat = new Regex("""\\begin\{[^}]*\}((([^\n]*\n))*)\\end\{[^}]*\}\n?""")
      Some(pat.replaceFirstIn(input, "$1"))
    }
  }

  val upperCaseFilter: SessionCommand = new Filter {
    protected override def transform(input: String, cwd: Path): Option[String] = Some(input.toUpperCase())
  }

  val exchangeMark: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      if (session.hasSelection) Some(new StateChange {
        val oldCursor = session.cursor
        val oldSelection = session.selection

        def undo(): Unit = {
          session.cursor = oldCursor; session.setMark(oldSelection.mark)
        }

        def redo(): Unit = {
          session.cursor = oldSelection.mark; session.setMark(oldCursor)
        }

        locally {
          redo()
        }
      }) else None
    }
  }

  import Utils.OffEdtThread

  val latexToPDF: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val CWD = session.CWD.toFile // the session's working directory
      val source = session.path // the file being edited
      val driver = session.TEX.toString // the driver file (to be latexed at the top level)
      val (row, _) = session.getCursorPosition // the 0-origin row of the file being edited
      val logWindow = Sessions.startSession(s"$driver.texlog", log = true)
      val logSession = logWindow.session

      locally {
        logSession.makeEphemeral()
        logSession.clear()
        logSession.insert(s"Running redpdf ${Utils.dateString()}\n")
        logWindow.gui.makeVisible()
      }

      /**
       * Send `out` to the logging window, and make
       * the latter visible immediately.
       */
      def toLogWindow(out: String): Unit = {
        if (!out.contains("/fonts/")) { // hack to be generalized
          logSession.insert(out)
          logSession.insert('\n')
          logSession.notifyHandlers()
        }
        ()
      }

      val offEdt = new OffEdtThread[Unit, String](toLogWindow(_), {
        logWindow.gui.makeVisible()
      }) {
        /**
         * The offEdt thread `publish`es output to its `stdout` and `stderr` streams.*
         * This is buffered, and from time to time it is passed to `log`
         */
        def logger = ProcessLogger(publish(_), publish(_))

        /**
         * This method runs in an offEdt thread; ie not on the EDT.
         * It invokes a Posix process that's run with the editing session's
         * working directory.
         */
        def doOffEDT() = {
          try {
            val cmd = List("redpdf", driver, (row + 1).toString, source)
            val process = Process(cmd, CWD)
            val exit = (process #< inputStreamOf("")) ! logger
            if (exit != 0) {
              publish(s"${cmd.mkString(" ")} [exit $exit]")
            }
          }
          catch {
            case exn: Exception => publish(exn.toString)
          }
        }
      }
      offEdt.execute() // Start the off edt thread; don't wait
      None
    }
  }

  val unicode: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      def replace(thePattern: String, theReplacement: String): Unit = {
        session.delete(-thePattern.length)
        session.insert(theReplacement)
        session.setMark(session.cursor, true)
        session.deSelect()
      }

      if (session.cursor > 0) {
        val theChar = session.document.characters.charAt(session.cursor - 1)
        val oldSelection = session.selection
        val thePattern = s"$theChar"
        val theReplacement = "\\u%04x".format(theChar.toInt)
        replace(thePattern, theReplacement)
        Some(new StateChange {
          def undo(): Unit = {
            replace(theReplacement, thePattern); session.selection = oldSelection
          }

          def redo(): Unit = {
            replace(thePattern, theReplacement)
          }
        })
      } else None
    }
  }

  /**
   *  If the text ending at the cursor matches ''any'' abbreviation, then
   *  replace that abbreviation with the text it abbreviates. Unicode
   *  sequences of the form `\uxxxx` are taken to be abbreviations for
   *  the characters they encode.
   *
   *  TODO: Unify the idea of an abbreviation and the Latex begin
   *        block specifications:
   *
   *        1. a simble abbr / block should be as now -- literal back-find
   *           to literal replace
   *
   *        2. a template abbr / block should treat the replacement text
   *           as a template, substituting for variables/expressions within
   *           it. For example, `${(F)}`, `${(R)}``${(A)}` could mean
   *           the content of the find pattern, replace, and argument
   *           fiels of the current session. The question remains:
   *           should it be possible to refer to the ''selection''
   *           in an ''abbreviation'' template invoked from the
   *           keyboard? Probably not: an abbreviation is "usually"
   *           typed just before the ABBREVIATE key, and the typing
   *           would eliminate the selection.
   *
   */
  val abbreviate: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      def replace(thePattern: String, theReplacement: String): Unit = {
        session.delete(-thePattern.length)
        session.insert(theReplacement)
        session.setMark(session.cursor, true)
        session.deSelect()
      }

      Red.Personalised.Bindings.longestSuffixMatch(session.document.characters, session.cursor) match {
        case None =>
          // it may be a unicode
          if (session.cursor > 6) {
            import Useful.CharSequenceOperations._
            val cursor = session.cursor
            val chars = session.document.characters
            val thePattern = chars.subSequence(cursor - 6, cursor).toString
            thePattern.toUnicode match {
              case None => None
              case Some(char) =>
                val oldSelection = session.selection
                val theReplacement = s"$char"
                replace(thePattern, theReplacement)
                Some(new StateChange {
                  def undo(): Unit = {
                    replace(theReplacement, thePattern); session.selection = oldSelection
                  }

                  def redo(): Unit = {
                    replace(thePattern, theReplacement)
                  }
                })
            }
          } else None

        case Some((theReplacement, length)) =>
          val oldSelection = session.selection
          val thePattern = session.document.characters.subSequence(session.cursor - length, session.cursor).toString
          replace(thePattern, theReplacement)
          Some(new StateChange {
            def undo(): Unit = {
              replace(theReplacement, thePattern); session.selection = oldSelection
            }

            def redo(): Unit = {
              replace(thePattern, theReplacement)
            }
          })
      }
    }
  }

  val pandocToPDF: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val CWD = session.CWD.toFile // the session's working directory
      val source = session.path // the file being edited
      val driver = source // the driver file
      val (row, _) = session.getCursorPosition // the 0-origin row of the file being edited
      val logWindow = Sessions.startSession(s"$driver.pandoclog", log = true)
      val logSession = logWindow.session

      locally {
        logSession.makeEphemeral()
        logSession.clear()
        logSession.insert(s"Running redpandoc ${Utils.dateString()}\n")
        logWindow.gui.makeVisible()
      }

      /**
       * Send `out` to the logging window, and make
       * the latter visible immediately.
       */
      def toLogWindow(out: String): Unit = {
        if (true) { // hack to be generalized
          logSession.insert(out)
          logSession.insert('\n')
          logSession.notifyHandlers()
        }
        ()
      }

      val offEdt = new OffEdtThread[Unit, String](toLogWindow(_), {
        logWindow.gui.makeVisible()
      }) {
        /**
         * The offEdt thread `publish`es output to its `stdout` and `stderr` streams.*
         * This is buffered, and from time to time it is passed to `log`
         */
        def logger = ProcessLogger(publish(_), publish(_))

        /**
         * This method runs in an offEdt thread; ie not on the EDT.
         * It invokes a Posix process that's run with the editing session's
         * working directory.
         */
        def doOffEDT() = {
          try {
            val cmd = List("redpandoc", driver, (row + 1).toString, source)
            val process = Process(cmd, CWD)
            val exit = (process #< inputStreamOf("")) ! logger
            if (exit != 0) {
              publish(s"${cmd.mkString(" ")} [exit $exit]")
            }
          }
          catch {
            case exn: Exception => publish(exn.toString)
          }
        }
      }
      offEdt.execute() // Start the off edt thread; don't wait
      None
    }
  }

  /** Use of tab button with or without selection */
  val indentOrTab = autoIndentSelection ||| autoTab
}
