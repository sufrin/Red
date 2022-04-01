package Jed

import Red._
import UserInputHandlers._
import Commands._
import InputEvent.Key
import InputEvent.Modifiers._

import java.awt.Color
import scala.swing._
import BorderPanel.Position._

class UI(val theSession: EditSession) extends SimpleSwingApplication {

  /**
   *  `theSession` emits warnings about things like find/replace failures
   *  that we wish to report via the user interface.
   */
  locally {
    theSession.warnings.handleWith { case (from, message) => warning(from, message) }
  }
  /** The source of handlers for user input events.  */
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
      icon = Settings.redIcon
    )
  }

  /**
   *  A text field in which feedback about the editing session is placed
   *  after every user command.
   */
  private val theFeedback = new TextField(40) {
    font = Settings.feedbackFont
    horizontalAlignment = Alignment.Center
    text = ""
    enabled = true
    peer.setForeground(Settings.feedbackColor)
  }

  /**
   * Place the given `message` in the feedback field, along
   * with details about the editing session.
   */
  def feedback(message: String): Unit = {
    val (row, col) = theSession.getCursorPosition
    val changed = if (theSession.document.hasChanged)  " (✖) " else " (✓) "
    theFeedback.text = s"$message ${theSession.path}@$row:$col $changed [${theSession.cursor}/${theSession.document.textLength}]"
  }

  /** The view in which `theSession`'s document will be shown.
   *  It is `theSession` itself that notifies
   *  */
  private val theView = new DocumentView(theSession) {
    preferredSize = defaultSize()
    focusable = true
  }

  /** The history manager for the sessions. It responds to DO/UNDO
   * commands as described in its specification
   */
  private val history = new Command.StateChangeHistory(theSession)
  locally {
    history.handleWith {
      case (done, undone) =>
        undoButton.enabled = done>0
        redoButton.enabled = undone>0
    }
    history.notifyHandlers()
  }

  /** An undo button */
  private val undoButton   = Button("\u25c0") { UI_DO(history.UNDO) } // (<) ◀
  /** A redo button */
  private val redoButton   = Button("\u25ba") { UI_DO(history.REDO) } // (>) ►


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
   *  to Find and Replace. This is the handler that implements the mapping.
   *  It should be added to existing handlers for the view and the text lines.
   */
  val findreplHandler: UserInputHandler = {
    case Instruction(Key.F, _, mods)  => find(findLine.text, backwards = mods.hasShift)
    case Instruction(Key.R, _, mods)  => replace(findLine.text, replLine.text, backwards = mods.hasShift)
  }

  private val findLine: TextLine  = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
  }
  private val replLine: TextLine  = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
  }

  private val theWidgets = new  BoxPanel(Orientation.Horizontal) {
    contents += Button("\u24bb") { find(findLine.text, false) } // (F)
    contents += findLine
    contents += Button("\u24c7") { replace(findLine.text, replLine.text, false )} // (R)
    contents += replLine
    contents += undoButton
    contents += redoButton

    // If the session has a cut ring feature, then make a button to start its UI
    if (theSession.hasCutRing)
        contents += Button("Cut Ring") {
          CutRingUI.refreshIfVisible()
        }
  }

  private val thePanel = new BorderPanel {
    layout(theWidgets)  = North
    layout(theView)     = Center
    layout(theFeedback) = South
  }

  def find(thePattern: String, backwards: Boolean): Unit = {
      UI_DO(EditSessionCommands.find(thePattern, backwards))
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean): Unit = {
      UI_DO(EditSessionCommands.replace(thePattern, theReplacement, backwards))
  }

  val top: Frame = new MainFrame() {
      title = s"Jedi: ${theSession.path}"
      contents = thePanel
      // menuBar = theMenuBar
      background = Color.lightGray

      theView.keystrokeInput.handleWith {
          handlers.mouse       orElse
          findreplHandler      orElse
          handlers.keyboard    orElse {
            case Instruction(Key.Z, _, ControlShift) => UI_DO(history.REDO)
            case Instruction(Key.Z, _, Control)      => UI_DO(history.UNDO)
            case other: UserInput                    => Logging.Default.info(s"Unhandled user input [[$other]] ")
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
     *  Invoked when the window closes.
     */
    override def closeOperation(): Unit = {}

  }

  def isVisible: Boolean = top.visible

  def start(): Unit = main(Array())
}

object UI extends Logging.Loggable
