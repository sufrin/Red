package Jed

import Commands._
import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import Red.UserInputHandlers._
import Red._

import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.swing._

class UI(val theSession: EditSession) extends SimpleSwingApplication {

  /**
   * `theSession` emits warnings about things like find/replace failures
   * that we wish to report via this user interface.
   *
   * Filters emit warnings about failure of external unix processes
   */
  locally {
    theSession.warnings.handleWith {
        case (from, message) => warning(from, message)
    }
    Filter.warnings.handleWith {
      case (from, message) => warning(from, message)
    }
  }
  /** The source of handlers for user input events. */
  protected val handlers = new EditSessionHandlers(UI_DO)

  /**
   * Show a popup window, entitled `from`, with a warning message in it.
   */
  def warning(from: String, message: String): Unit = {
    Dialog.showMessage(
      theView,
      message,
      from,
      Dialog.Message.Warning,
      icon = Utils.redIcon
    )
  }

  /**
   * A text field in which feedback about the editing session is placed
   * after every user command.
   */
  private val theFeedback = new TextField(40) {
    font = Utils.feedbackFont
    horizontalAlignment = Alignment.Center
    text = ""
    enabled = true
    peer.setForeground(Utils.feedbackColor)
  }

  /**
   * Place the given `message` in the feedback field, along
   * with details about the editing session.
   */
  def feedback(message: String): Unit = {
    val (row, col) = theSession.getCursorPosition
    val changed = if (hasChanged) " (✖) " else " (✓) "
    theFeedback.text =
      s"$message ${theSession.path}@$row:$col $changed [${theSession.cursor}/${theSession.document.textLength}]"
  }

  /** The component in which `theSession`'s document will be shown.
   *  It is `theSession` that notifies this component of cursor,
   *  selection, or document changes.
   */
  private val theView = new DocumentView(theSession) {
    preferredSize = defaultSize()
    focusable     = true
  }

  /** The history manager for `theSession`. It responds to DO/UNDO
   *  commands as described in its specification
   */
  private val history = new Command.StateChangeHistory(theSession)
  /** An undo button */
  private val undoButton = Button("\u25c0") { UI_DO(history.UNDO) } // ◀
  /** A redo button */
  private val redoButton = Button("\u25ba") { UI_DO(history.REDO) } // ►

  locally {
    history.handleWith {
      case (done, undone) =>
        undoButton.enabled = done > 0
        redoButton.enabled = undone > 0
    }
    undoButton.enabled = false
    redoButton.enabled = false
  }

  /**
   *
   * Execute the given command, `c` under supervision of
   * the history manager, then present the feedback, and
   * finally make the session notify all handlers if
   * any changes have been made in the session. It is
   * this final method call that causes `theView` to
   * be synchronised with the session.
   */
  def UI_DO(c: Commands.Command[EditSession]): Unit = {
    history.DO(c)
    feedback("")
    theSession.notifyHandlers()
  }

  /** The document view and the find and replace text lines all map Ctrl-F/Ctrl-R
   * to Find and Replace. This is the handler that implements the mapping.
   * It should be added to existing handlers for the view and the text lines.
   */
  val findreplHandler: UserInputHandler = {
    // Keypad bindings to find and replace
    case Instruction(Key.Decimal, _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)
    case Instruction(Key.Numpad0, _, mods) => find(findLine.text, backwards = mods.hasShift)

    case Instruction(Key.F, _, mods) => find(findLine.text, backwards = mods.hasShift)
    case Instruction(Key.R, _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)
  }

  private val argLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
    tooltip = "Argument(s) for some commands"
  }

  private val findLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
  }

  private val replLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
  }
  private val regexCheck: CheckBox = new CheckBox("") {
    tooltip  = "Match regular expression / Match literal"
  }

  /**
   *  The top line below the menu bar holds the find and replace
   *  buttons and text lines, as well as an undo and a redo button
   *  linked to the undo history
   */
  private val theWidgets = new BoxPanel(Orientation.Horizontal) {
    contents += Button(" \u24b6 ") {
      argLine.text = ""
    }
    contents += argLine
    contents += regexCheck
    contents += Button("\u24bb") {
      find(findLine.text, false)
    } // (F)
    contents += findLine
    contents += Button("\u24c7") {
      replace(findLine.text, replLine.text, false)
    } // (R)
    contents += replLine
    contents += undoButton
    contents += redoButton
  }

  private val theMenuBar: MenuBar = new MenuBar {
    def Item(name: String)(act: => Unit): MenuItem = new MenuItem(Action(name) {
      act
    }) {
      font = Utils.buttonFont
    }

    contents += new Menu("File") {

      contents += Item("New") {
        new Jedi(s"New@${Utils.dateString()}")
      }

      contents += Item("Open \u24b6") {
        val path = argLine.text
        new Jedi(path)
      }

      contents += Item("Save") {
        Retry.reset()
        theSession.path = Utils.save(theSession.path, theSession.document)
        feedback("Saved")
      }

      contents += Item("Close") {
        if (hasChanged && Retry.must)
          warning("Jedi", "Document may need saving: repeat if you're sure")
        else
          closeUI()
      }

      contents += Item("Quit") {
        if (hasChanged && Retry.must)
          warning("Jedi", "Document may need saving: repeat if you're sure")
        else
          sys.exit()
      }
    } // File Menu

    contents += new Menu("Edit") {
        contents += Item("Replace \u24bb with \u24c7 in the selection") {
          val asRegex = regexCheck.selected
          UI_DO(EditSessionCommands.replaceAllInSelection(findLine.text, replLine.text, asRegex))
        }

        contents += Separator()

        contents += Item("fmt ...") {
          UI_DO(EditSessionCommands.formatter(argLine.text))
        }

        contents += Item("LowerCase") {
          UI_DO(EditSessionCommands.lowerCaseFilter)
        }

        contents += Item("UpperCase") {
          UI_DO(EditSessionCommands.upperCaseFilter)
        }

        if (theSession.hasCutRing) {
          contents += Separator()
          contents += Item("Cut Ring") {
            CutRingUI.refreshIfVisible()
          }
        }
    } // Edit Menu

  } // theMenuBar

  private val thePanel = new BorderPanel {
    layout(theWidgets) = North
    layout(theView) = Center
    layout(theFeedback) = South
  }

  def find(thePattern: String, backwards: Boolean): Unit = {
    val asRegex = regexCheck.selected
    UI_DO(EditSessionCommands.find(thePattern, backwards, asRegex))
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean): Unit = {
    val asRegex = regexCheck.selected
    UI_DO(EditSessionCommands.replace(thePattern, theReplacement, backwards, asRegex))
  }

  val top: Frame = new MainFrame() {
    title = s"Jedi: ${theSession.path}"
    background = Color.lightGray
    contents = thePanel
    if (isFileEditor) menuBar = theMenuBar

    theView.keystrokeInput.handleWith {
        handlers.mouse    orElse
        findreplHandler   orElse
        handlers.keyboard orElse {
        case Instruction(Key.Z, _, ControlShift) => UI_DO(history.REDO)
        case Instruction(Key.Z, _, Control) => UI_DO(history.UNDO)
        case other: UserInput => Logging.Default.info(s"Unhandled user input [[$other]] ")
      }
    }

    locally {
      // Undocumented magic to place the window sensibly
      peer.setLocationByPlatform(true)
      // Undocumented magic to avoid window-closing shutting down the entire app
      peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
      // Initial feedback when the window opens
      reactions += { case event.WindowOpened(_) => feedback("")
      }
    }

    /**
     * Invoked when the window closes.
     */
    override def closeOperation(): Unit = warning("Jedi", "Use File/Close or File/Quit")

  }

  /** Has the document being edited here changed? */
  def hasChanged: Boolean = theSession.hasChanged

  /** Is this a genuine File-editing UI? It might be a cut ring UI,
   * which can dispense with parts of the "full" UI.
   */
  def isFileEditor: Boolean = theSession.hasCutRing

  /** Close this UI (and only this) */
  def closeUI(): Unit = top.close()

  /** Asking `Retry.must` yields true the first time, thereafter false,
   * until `Retry.reset()` */
  object Retry {
    private var _must: Boolean = true

    def reset(): Unit = {
      _must = true
    }

    def must: Boolean = {
      val reallyMust = _must; _must = false; reallyMust
    }
  }

  def isVisible: Boolean = top.visible


  def start(): Unit = main(Array())
}

object UI extends Logging.Loggable
