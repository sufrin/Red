package Jed

import Commands._

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

  def insert(ch: Char): SessionCommand = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = Some {
      session.insert(ch)
      new StateChange {
        def undo(): Unit = session.delete()
        def redo(): Unit = session.insert(ch)
        override val kind: String =
          if (ch == '\n') "InsEol" else "Ins" // break insertion merges
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
      session.setCursorAndMark(row, col)
      Some {
        new StateChange {
          def undo(): Unit = { session.selection = oldSelection; session.cursor = oldCursor }
          def redo(): Unit = session.setCursorAndMark(row, col)
        }
      }
    }
  }

  /**
   *  Dragging the cursor is unusual.
   *  There is no need for a history item, because a drag will always be
   *  preceded by a `setCursorAndMark`, whose  undo method will suffice
   *  to undo the whole press-drag sequence, but whose redo method does not
   *  redo the whole press-drag sequence -- simply restoring the cursor
   *  to where the drag started.
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



  
}
