package Jed

import Commands._
import Red.SystemClipboard

import scala.sys.process.Process

/**
 *   `Command`s derived from `EditSession` methods.
 *
 *   Commands are constants, except for those that
 *   are parameterised by characters (to insert), strings
 *   (for find/replace) or document positions.
 */
object EditSessionCommands extends Logging.Loggable {

  type SessionCommand    = Command[EditSession]
  type StateChangeOption = Option[StateChange]

  /**
   *  Insert a newline and (if the session is in `autoIndenting` mode)
   *  enough spaces to align the left margin to the indentation of
   *  the current line.
   */
  val autoIndentNL: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = Some {
      val indent = if (session.autoIndenting) session.currentIndent else 0
      new StateChange {
        def undo(): Unit = session.deleteFor(-(indent+1), record=false)
        def redo(): Unit = {
            session.insert('\n')
            for { i<-0 until indent } session.insert(' ')
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

  def indentSelectionBy(indentBy: String): SessionCommand =
    new Filter {
    override def adjustNL: Boolean = true
    override val kind: String = "indentSel"
    override def transform(input: String): Option[String] = {
      val lines = input.split('\n')
      val result= new StringBuilder()
      for { line <- lines } { result.addAll(indentBy); result.addAll(line); result.addOne('\n') }
      Some(result.toString())
    }
  }

  /**
   *  Remove the prefix `undentBy` from each line of the
   *  current selection. Pragmatically implemented.
   */
  val undentSelection: SessionCommand = undentSelectionBy(undentBy)

  def undentSelectionBy(undentBy: String): SessionCommand = new Filter {
    override def adjustNL: Boolean = true
    override val kind: String = "undentSel"
    override def transform(input: String): Option[String] = {
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

  def formatter(arg: String): SessionCommand = new Filter {
    def numOrSwitch(arg: String): Boolean = {
      arg.startsWith("-") || arg.matches("[0-9]+")
    }
    override def transform(input: String): Option[String] = {
      val args = FilterUtilities.parseArguments(arg)
      val cmd  = Process("fmt" :: args)//TODO: , new java.io.File(cwd))
      if (args forall numOrSwitch)
        Some(Filter.runProcess(cmd, input))
      else {
        Filter.warnings.notify("fmt", "fmt arguments in \u24b6 must be numbers or -switches")
        None
      }
    }
  }

  /**
   *   A utility that whose result is a `SessionCommand` that is equivalent to `command`
   *   if the session is in type-over-selection mode and there is a selection.
   *   Otherwise the result simply succeeds having done nothing.
   */
  def whenInTypeoverMode(command: SessionCommand): SessionCommand =
    Command.when((s: EditSession) => s.typeOverMode&&s.hasSelection, command)

  /**  A utility command that always succeeds after notifying the
   *   session's handlers that something in the session (may have) changed.
   *   When sequentially composing a session command from two or more others,
   *   it's usually necessary to synchronize the context (in particular
   *   the document display) with the session after all but the last session
   *   command.
   */
  def notifyNow: SessionCommand = new SessionCommand {
       def DO(session: EditSession): StateChangeOption = {
           session.notifyHandlers()
           Some(Command.undoNothing)
    }
  }

  /**
   * An insertion from the keyboard that cuts the selection first if
   * the session is in type-over-selection mode.
   */
  def insertCommand(ch: Char): SessionCommand = whenInTypeoverMode(cut &&& notifyNow) &&& insert(ch)

  def insert (ch: Char): SessionCommand = {
      if (ch == '\n') autoIndentNL else new SessionCommand {
        def DO(session: EditSession): StateChangeOption = {
          Some {
            session.insert(ch)
            new StateChange {
              def undo(): Unit = session.delete()

              def redo(): Unit = session.insert(ch)

              override val kind: String =
                if (ch == '\n') "InsEol" else "Ins" // break insertion merges
            }
          }
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
          def undo(): Unit = { session.selection = oldSelection; session.cursor = oldCursor }
          def redo(): Unit = {
              session.setCursorAndMark(row, col)
          }
          locally { redo() }
        }
      }
    }
  }

  /**
   *  Dragging the cursor is unusual.
   *  There is no need for a history item, because a drag will always be
   *  preceded by a `setCursorAndMark`, and followed by a `mouseUp`
   *  whose undo method will suffice to undo the whole drag sequence, and
   *  whose redo method simply restores the selection as it was at the point the mouse
   *  was released.
   *
   *  '''TL;DR '''
   *  In any case, having a history item per drag event would be
   *  costly in terms of space and bandwidth: even if the drags were (as they
   *  should be) merged.
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
      else Some (new StateChange {
          val oldCursor    = session.draggingFrom.get
          val oldSelection = session.selection
          session.stopDragging
          def undo(): Unit = { session.cursor = oldCursor; session.selection = NoSelection }
          def redo(): Unit = { session.cursor = oldSelection.cursor; session.setMark(oldSelection.mark) }
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

  def selectMatching(select: EditSession=>Boolean): SessionCommand = new SessionCommand {
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

  val selectMatchingUp: SessionCommand   = selectMatching(_.selectMatchingUp)
  val selectMatchingDown: SessionCommand = selectMatching(_.selectMatchingDown)
  val selectMatching: SessionCommand     = (selectMatchingUp ||| selectMatchingDown ||| Command.doNothing)

  val copy: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = if (session.hasNoSelection) None
    else {
      val oldSelection = session.selection
      val oldClip      = SystemClipboard.getOrElse("")
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
      val oldCursor    = session.cursor
      val oldClip      = SystemClipboard.getOrElse("")
      val oldSelected  = session.selectionText()
      Some {
        new StateChange {
          def undo(): Unit = {
            SystemClipboard.set(oldClip)
            // Should restore a selection with the correct polarity
            session.cursor    = oldSelection.left // reposition the session
            session.insert(oldSelected)           // insert the old selected text
            session.cursor    = oldSelection.cursor
            session.selection = Span(session.cursor, oldSelection.mark)
          }
          def redo(): Unit = {
            session.selection = oldSelection
            session.cursor    = oldCursor
            session.cut()
          }
          locally { redo() }
        }
      }
    }
  }

  val paste: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldSelection = session.selection
      val oldCursor    = session.cursor
      val oldClip      = SystemClipboard.getOrElse("")
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
      val oldCursor    = session.cursor
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
      val oldCursor    = session.cursor
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
      val oldCursor    = session.cursor
      session.selectAll()
      Some {
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor    = oldCursor
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
      val oldCursor    = session.cursor
      session.selectChunk(row, col, clicks)
      Some {
        val newCursor    = session.cursor
        val newSelection = session.selection
        new StateChange {
          def undo(): Unit = {
            session.selection = oldSelection
            session.cursor    = oldCursor
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


  val clearAll: SessionCommand = Command.andThen(selectAll, cut)

  def exchangeCut: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      val oldClip         = SystemClipboard.getOrElse("")
      val oldSelection    = session.selection             // get the polarity right on undo
      val oldSelected     = session.exch(oldClip)
      Some {
        new StateChange {
          def undo(): Unit = { session.exch(oldSelected); session.selection = oldSelection }
          def redo(): Unit = session.exch(oldClip)
        }
      }
    }
  }

  def find(thePattern: String, backwards: Boolean, asRegex: Boolean): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
        val oldSelection = session.selection
        val oldCursor = session.cursor
        if (session.find(thePattern, backwards, asRegex)) Some (new StateChange {
           def undo(): Unit = { session.cursor=oldCursor; session.selection = oldSelection}
           def redo(): Unit = session.find(thePattern, backwards, asRegex)
        }) else None
    }
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean, asRegex: Boolean) : SessionCommand =
      new SessionCommand {
        def DO(session: EditSession): StateChangeOption = {
          val replaced = session.replace(thePattern, theReplacement, backwards, asRegex)
          if (replaced.isDefined) Some (new StateChange {
            def undo(): Unit = { session.exch(replaced.get)  }
            def redo(): Unit = session.replace(thePattern, theReplacement, backwards, asRegex)
          }) else None
        }
      }

  def replaceAllInSelection(thePattern: String, theReplacement: String, asRegex: Boolean): SessionCommand =
    new SessionCommand {
      def DO(session: EditSession): StateChangeOption = {
        val replaced = session.replaceAllInSelection(thePattern, theReplacement, asRegex)
        if (replaced.isDefined) Some (new StateChange {
          def undo(): Unit = { session.exch(replaced.get)  }
          def redo(): Unit = session.replaceAllInSelection(thePattern, theReplacement, asRegex)
        }) else None
      }
    }

  val lowerCaseFilter: SessionCommand = new Filter {
    override def transform(input: String): Option[String] = Some(input.toLowerCase())
  }

  val upperCaseFilter: SessionCommand = new Filter {
    override def transform(input: String): Option[String] = Some(input.toUpperCase())
  }

  val exchangeMark: SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      if (session.hasSelection) Some (new StateChange {
        val oldCursor    = session.cursor
        val oldSelection = session.selection
        def undo(): Unit = { session.cursor = oldCursor; session.setMark(oldSelection.mark)  }
        def redo(): Unit = { session.cursor = oldSelection.mark; session.setMark(oldCursor)  }
        locally { redo() }
      }) else None
    }
  }
}
