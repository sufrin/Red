package Jed

import Red._
import InputEventDetail.Key
import InputEventDetail.ImplicitExtensions
import InputEventDetail.Modifiers._

import java.awt.Color
import scala.swing._
import BorderPanel.Position._

class UI(val theSession: EditSession) extends SimpleSwingApplication {
  type UserInputHandler = Notifier.Handler[UserInput]

  def warning(from: String, message: String): Unit = {
    Dialog.showMessage(
      theView,
      message,
      from,
      Dialog.Message.Warning,
      icon = Settings.redIcon
    )
  }

  private val theFeedback = new TextField(40) {
    font = Settings.feedbackFont
    horizontalAlignment = Alignment.Center
    text = ""
    enabled = true
    peer.setForeground(Settings.feedbackColor)
  }

  def feedback(message: String): Unit = {
    val (row, col) = theSession.getCursorPosition
    theFeedback.text = s"$message ${theSession.path}@$row:$col [${theSession.cursor}/${theSession.document.textLength}]"
  }

  private val theView = new DocumentView(theSession) {
    preferredSize = defaultSize()
    focusable = true
  }

  val handlers = new EditSessionHandlers(DoInUI)

  private val undoButton   = Button("\u25c0") { DoInUI(history.UNDO) } // (<)
  private val redoButton   = Button("\u25ba") { DoInUI(history.REDO) } // (>)

  private val history = new Commands.Command.StateChangeHistory(theSession)
  locally {
    history.handleWith {
      case (done, undone) =>
        undoButton.enabled = done>0
        redoButton.enabled = undone>0
    }
    history.notifyHandlers()
  }

  private val findLine      = new Jed.TextLine(25) {
    override def firstHandler: UserInputHandler = handlers.logAll(log=UI.log) orElse {
      case Character('\n', _, mods)       => find(this.text, backwards=mods.hasShift)
      case Character('\r', _, Shift)      => find(this.text, backwards=true)
    }
  }

  private val replLine = new Jed.TextLine(25) {
    override def firstHandler: UserInputHandler = handlers.logAll(log=UI.log) orElse {
      case Character('\n', _, mods)       => replace(findLine.text, this.text, backwards=mods.hasShift)
      case Character('\r', _, Shift)      => replace(findLine.text, this.text, backwards=true)
    }
  }

  private val theWidgets = new  BoxPanel(Orientation.Horizontal) {
    contents += Button("\u24bb") { find(findLine.text, false) } // (F)
    contents += findLine
    contents += Button("\u24c7") { replace(findLine.text, replLine.text, false )} // (R)
    contents += replLine
    contents += undoButton
    contents += redoButton
  }

  private val thePanel = new BorderPanel {
    layout(theWidgets)  = North
    layout(theView)     = Center
    layout(theFeedback) = South
  }

  def find(thePattern: String, backwards: Boolean): Unit = {
      warning("Find", s"unimplemented: find $thePattern ($backwards)")
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean): Unit = {
    warning("Replace", s"unimplemented: replace $thePattern/$theReplacement ($backwards)")
  }

  def DoInUI(c: Commands.Command[EditSession]): Unit = {
    history.DO(c)
    feedback("")
    theSession.notifyHandlers()
  }

  val top: Frame = new MainFrame() {
      title = "Jedi"
      contents = thePanel
      // menuBar = theMenuBar
      background = Color.lightGray

      theView.keystrokeInput.handleWithTagged("UI") {
          handlers.mouse       orElse
          handlers.keyboard    orElse {
            case Instruction(Key.Z, _, ControlShift) => DoInUI(history.REDO)
            case Instruction(Key.Z, _, Control)      => DoInUI(history.UNDO)
            case other: UserInput                    => Logging.Default.info(s"Unhandled user input [[$other]] ")
          }
      }
  }

  def start(): Unit = main(Array())
}

object UI extends Logging.Loggable
