package Jed

import Commands._
import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import Red.UserInputHandlers._
import Red._

import java.awt.Color
import java.nio.file.{Files, Path, Paths}
import scala.swing.BorderPanel.Position._
import scala.swing.FileChooser.Result.{Approve, Cancel}
import scala.swing._

class UI(val theSession: EditSession) extends SimpleSwingApplication {

  /**
   * `theSession` emits warnings about things like find/replace failures
   * that we wish to report via this user interface.
   */
  locally {
    theSession.warnings.handleWith {
        case (from, message) => warning(from, message)
    }
  }

  /**
   * A warning from Filter during the execution of  `body` is reported
   * only in the present UI.
   */
  def withFilterWarnings (title: String) (body: => Unit): Unit = {
    Filter.warnings.handleOnce(title) {
      case (source, message) => warning(title, message)
    } { body }
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
      s"$message ${theSession.displayPath}@${row+1}:$col $changed [${theSession.cursor}/${theSession.document.textLength}]"
  }

  def feedbackTemporarily(message: String): Unit = {
    theFeedback.text = message
  }

  def feedbackWD(wd: String) = {
    theFeedback.text = s"cwd: $wd"
  }

  def longFeedback(msg: String): Unit = {
    val message      = new StringBuffer()
    val path         = Paths.get(theSession.path)
    val lastModified = try Files.getLastModifiedTime(path).toMillis catch { case _: Exception => 0L }
    val exists       = Files.exists(path)
    message append msg
    message append " "
    message append theSession.displayPath
    message append (if (theSession.hasChanged) " (✖) " else " (✓) ")
    message append (if (Files.isReadable(path)) "+R" else "-R")
    message append (if (Files.isWritable(path)) "+W" else "-W")
    message append " "
    message.append(
      if (lastModified == 0)
        s"[NEW ${Utils.dateString()}]"
      else
        Utils.dateString(lastModified)
    )
    message.append(s" [${theSession.cursor}/${theSession.document.textLength}]")
    theFeedback.text = message.toString
    if (top != null) top.title = s"Red: ${theSession.displayPath} ${if (exists) "" else "[NEW]"}"
  }

  /** The component in which `theSession`'s document will be shown.
   *  It is `theSession` that notifies this component of cursor,
   *  selection, or document changes.
   */
  private val theView = new DocumentView(theSession) {
    preferredSize = defaultSize()
    focusable     = true
  }

  def SmallButton(label: String, toolTip: String="")(act: => Unit): Button = new Button(new Action(label) { def apply(): Unit = act } ) {
    font           = Utils.smallButtonFont
    val metrics    = peer.getFontMetrics(font)
    val labWidth   = metrics.stringWidth(label)
    val charHeight = metrics.getHeight
    maximumSize    = new Dimension(labWidth, charHeight)
    preferredSize  = maximumSize
    if (toolTip.nonEmpty) tooltip=toolTip
  }

  def Button(label: String, toolTip: String="")(act: => Unit): Button = new Button(new Action(label) { def apply(): Unit = act } ) {
    if (false) {
      font = Utils.buttonFont
      val metrics = peer.getFontMetrics(font)
      val labWidth = metrics.stringWidth(label)
      val charHeight = metrics.getHeight
      maximumSize = new Dimension(labWidth, charHeight)
      preferredSize = maximumSize
    }
    if (toolTip.nonEmpty) tooltip=toolTip
  }


  /** The history manager for `theSession`. It responds to DO/UNDO
   *  commands as described in its specification
   */
  private val history = new Command.StateChangeHistory(theSession)
  /** An undo button */
  private val undoButton = SmallButton("\u2770", toolTip="Undo") { UI_DO(history.UNDO) } //
  /** A redo button */
  private val redoButton = SmallButton("\u2771", toolTip="Redo") { UI_DO(history.REDO) } //

  locally {
    history.handleWith {
      case (done, undone) =>
        undoButton.enabled = done > 0
        redoButton.enabled = undone > 0
    }
    undoButton.enabled = false
    redoButton.enabled = false
    undoButton.font=Utils.feedbackFont
    redoButton.font=Utils.feedbackFont
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

    case Instruction(Key.Decimal, _, mods) if (mods.hasControl) =>
      replLine.text=""
      replLine.requestFocusInWindow()

    case Instruction(Key.Decimal, _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)

    case Instruction(Key.Numpad0, _, mods) if (mods.hasControl) =>
      findLine.text = ""
      findLine.requestFocusInWindow()

    case Instruction(Key.Numpad0, _, mods)  =>
      if (mods.hasAlt && theSession.hasSelection) findLine.text = theSession.selectionText()
      find(findLine.text, backwards = mods.hasShift)

    case Instruction(Key.F, _, mods)  =>
      if (mods.hasAlt && theSession.hasSelection) findLine.text = theSession.selectionText()
      find(findLine.text, backwards = mods.hasShift)

    case Instruction(Key.R, _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)
  }

  private val argLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = {
      case Instruction(Key.G, _, mods) if mods.hasControl =>
        val loc = argLine.text.strip()
        goTo(loc)

      case Instruction(Key.Q, _, Control) =>
        top.closeOperation()

      case Instruction(Key.S, _, Control) =>
        saveOperation()

      case Instruction(Key.E, _, Control) =>
        openArglinePath()
    }
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()

    tooltip = "Argument(s) for some commands"
  }

  private val findLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()
  }

  private val replLine: TextLine = new TextLine(25) {
    override def firstHandler: UserInputHandler = findreplHandler
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()
  }

  private val regexCheck: CheckBox = new CheckBox("") {
    tooltip  = "Treat \u24bb pattern as regular expression (or literal)" // (F)
  }

  /**
   *  The top line below the menu bar holds the find and replace
   *  buttons and text lines, as well as an undo and a redo button
   *  linked to the undo history
   */
  private val theWidgets = new BoxPanel(Orientation.Horizontal) {
    contents += Button(" \u24b6 ", toolTip = "(Clear) the \u24b6 field \u2191") {
      argLine.text = ""
    } // (A)
    contents += argLine
    contents += regexCheck
    contents += Button("\u24bb", toolTip = "Find the pattern \u2191") {
      find(findLine.text, false)
    } // (F)
    contents += findLine
    contents += Button("\u24c7", toolTip = "Replace the matched pattern with the template (or literal text) \u2191") {
      replace(findLine.text, replLine.text, false)
    } // (R)
    contents += replLine
  }

  // TODO: Eventually this should be a user-preference module
  private object Settings {
    var typeOverSelection: Boolean = false
    var clickSelects:      Boolean = true
    var autoIndenting:     Boolean = true
  }

  private val theMenuBar: MenuBar = new MenuBar {
    def Item(name: String)(act: => Unit): MenuItem = new MenuItem(Action(name) {
      act
    }) {
      font = Utils.buttonFont
    }

    contents += new Menu("Red") {

      contents += Item("cd \u24b6") { theSession.CWD=Utils.toPath(argLine.text.strip()); feedbackWD(theSession.CWD.toString) }
      contents += Item("cd +")      { theSession.CWD=theSession.parentPath; feedbackWD(theSession.CWD.toString) }
      contents += Item("cd ~")      { theSession.CWD=Utils.homePath; feedbackWD(theSession.CWD.toString) }

      contents += Separator()
      contents += Separator()

      contents += new CheckMenuItem("Type over selection") {
        tooltip  = "When enabled, the selection is cut when material is typed"
        font     = Utils.buttonFont
        selected = Settings.typeOverSelection
        listenTo(this)
        reactions += {
          case event.ButtonClicked(_) =>
            theSession.typeOverSelection = selected
        }
      }

      contents += new CheckMenuItem("Select adjacent bracket scope") {
        tooltip  = "When enabled, a mouse-click adjacent to bracketed material selects that material"
        font     = Utils.buttonFont
        selected = Settings.clickSelects
        listenTo(this)
        reactions += {
          case event.ButtonClicked(_) =>
            Settings.clickSelects = selected
        }
      }

      contents += new CheckMenuItem("Auto indent") {
        tooltip  = "When enabled, a newline is followed by enough spaces to align the start of the current line"
        font     = Utils.buttonFont
        selected = Settings.autoIndenting
        listenTo(this)
        reactions += {
          case event.ButtonClicked(_) =>
            theSession.autoIndenting = selected
        }
      }

      contents += Separator()
      contents += Separator()

      contents += Item("Quit")  { top.closeOperation() }

    }

    contents += new Menu("File") {

      contents += Item("New") {
        openFileRequests.notify(s"${theSession.CWD.toString}/New=${Utils.dateString()}")
      }

      contents += Item("Open \u24b6") {
        openArglinePath()
      }

      contents += Item("Save") {
        saveOperation()
      }

      contents += Item("Save as \u24b6") {
        val text = argLine.text.trim
        if (text.isEmpty)
          {
            fileChooser.showSaveDialog(top) match {
              case Cancel  =>
                feedbackTemporarily("Save as: no path specified")
              case Approve =>
                val path = fileChooser.selectedFile.getAbsolutePath
                top.saveAs(path.toString)
            }
          }
        else
        top.saveAs(text)
      }

      contents += Item("Save & Quit") {
        tooltip = "Save the document if it needs saving; then close this session."
        close()
      }

    } // File Menu

    contents += new Menu("Edit") {
        contents += Item("Replace \u24bb with \u24c7 in the selection") {
          val asRegex = regexCheck.selected
          UI_DO(EditSessionCommands.replaceAllInSelection(findLine.text, replLine.text, asRegex))
        }

        contents += Separator()

        contents += Item("fmt ...") {
          withFilterWarnings("fmt ") { UI_DO(EditSessionCommands.formatter(argLine.text)) }
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

    contents += new Menu("Pipe") {
      // Pipe the selection through ...
      contents += Button("\u24b6 « sel'n") {
        withFilterWarnings("\u24b6 « sel'n") { UI_DO(EditSessionCommands.pipeThrough(argLine.text))}
      }

      contents += Button("wc « sel'n") {
        withFilterWarnings("wc « sel'n") { UI_DO(EditSessionCommands.pipeThrough("wc")) }
      }

      contents += Button("ls \u24b6 « sel'n") {
        withFilterWarnings("ls \u24b6 « sel'n") { UI_DO(EditSessionCommands.pipeThrough(s"ls ${argLine.text}")) }
      }


    } // Pipe Menu

    contents += Button("PDF", toolTip = "Run redpdf now") { UI_DO(EditSessionCommands.latexToPDF) }
    contents += Glue.horizontal()
    contents += undoButton
    contents += redoButton

  } // theMenuBar

  private val thePanel = new BorderPanel {
    layout(theWidgets)  = North
    layout(theView)     = Center
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

  val top: ControllingFrame = new ControllingFrame(theSession) {
    title = s"Jedi: ${theSession.path}"
    background = Color.lightGray
    contents = thePanel
    if (isFileEditor) menuBar = theMenuBar

    /** Mouse down and (maybe) select an adjacent bracketed text */
    val mouseDown: UserInputHandler = {
      case MousePressed(row, col, 1, Button1) =>
        UI_DO(EditSessionCommands.setCursorAndMark(row, col))
        UI_DO(EditSessionCommands.selectMatching)
    }

    val indentKeys: UserInputHandler = {
      case Instruction(Key.Tab, _, Alt)       =>
           UI_DO(EditSessionCommands.indentSelectionBy(argLine.text))
      case Instruction(Key.Tab, _, AltShift)  =>
           UI_DO(EditSessionCommands.undentSelectionBy(argLine.text))
    }

    theView.keystrokeInput.handleWith {
        indentKeys        orElse
        mouseDown         orElse
        handlers.mouse    orElse
        findreplHandler   orElse
        handlers.keyboard orElse {
        case Instruction(Key.Z, _, ControlShift) => UI_DO(history.REDO)
        case Instruction(Key.Z, _, Control) => UI_DO(history.UNDO)
        case Instruction(Key.Q, _, Control) =>
          top.closeOperation()
        case Instruction(Key.S, _, Control) =>
          saveOperation()
        case other: UserInput =>
          Logging.Default.info(s"Unhandled user input [[$other]] ")
      }
    }

    locally {
      reactions += { case event.WindowOpened(_) => longFeedback("")
      }
    }

    def saveAs(aPath: String): Boolean = {
      Utils.save(aPath, theSession.document) match {
        case None =>
          theSession.path = aPath
          longFeedback("Saved")
          true
        case Some(errorMessage) =>
          warning(s"Saving as: $aPath", errorMessage)
          longFeedback("Unsaved")
          false
      }
    }
  }

  /** Has the document being edited here changed? */
  def hasChanged: Boolean = theSession.hasChanged

  /** Is this a genuine File-editing UI? It might be a cut ring UI,
   * which can dispense with parts of the "full" UI.
   */
  def isFileEditor: Boolean = theSession.hasCutRing

  /** Close this UI (and only this) */
  def closeUI(): Unit = top.close()

  /**
   *   Is the top-level window of this GUI visible?
   *   If it isn't then there might be a
   *   performance advantage in avoiding
   *   updating it. (See the prototype Cut Ring GUI
   *   for an example).
   */
  def isVisible: Boolean = top.visible

  /**
   * Force the top-level window of this GUI to
   * become visible.
   */
  def makeVisible(): Unit = {
    top.uniconify()
    top.peer.toFront()
  }

  /**
   * Go to the location denoted by `location` as a `lineNumber x columnNumber` pair
   * with separator `,` or `:`, or `.`. If the separator and column number
   * are missing then take the column number as 0.
   */
  def goTo(location: String): Unit = {
    if (location != "") {
      val (r, c) =
        location match {
          case s"$ln,$cn" => (ln.toIntOption, cn.toIntOption)
          case s"$ln:$cn" => (ln.toIntOption, cn.toIntOption)
          case s"$ln.$cn" => (ln.toIntOption, cn.toIntOption)
          case s"$ln"     => (ln.toIntOption, Some(0))
        }
      (r, c) match {
        case (Some(ln), Some(cn)) =>
          UI_DO(EditSessionCommands.setCursorAndMark(ln - 1, cn))
        case (_, _) =>
          warning("GoTo", s"Not a location: $location\nUse #.# or #,# or #:# or #")
      }
    }
  }


  /**  Notifies that this GUI and its associated
   *   session have (already) been closed. Alias
   *   for `top.sessionClosed`.
   */
  val sessionClosed: Notifier[String] = top.sessionClosed

  /**  Notifies requests
   *   (from the `File` menu) to open files.
   */
  val openFileRequests = new Notifier[String]

  /**
   *  Force the top-level window to behave as if its
   *  close button has been clicked. This will not actually
   *  close the window or the session if there are
   *  editing changes and the user elects to carry
   *  on editing.
   */
  def close(): Unit = top.closeOperation()

  /**
   *  Open the file designated by the text on the argument line.
   *  Prefixes such as `~/` and `+/` are treated in the usual way:
   *  the former means the home directory, the latter means the
   *  parent directory of the file being edited from this UI.
   */
  def openArglinePath(): Unit = {
  val text = argLine.text.trim
  val path = text match {
    case s"/$rest" => Paths.get(text).toString
    case s"~/$rest" => Utils.homePath.resolve(rest).toString
    case s"+/$rest" => theSession.parentPath.resolve(rest).toString
    case "" =>
      fileChooser.showOpenDialog(top) match {
        case Cancel  => ""
        case Approve => fileChooser.selectedFile.getAbsolutePath.toString
      }
    case _ => theSession.CWD.resolve(text).toString
  }
  if (path.nonEmpty) openFileRequests.notify(path) else feedbackTemporarily("Open: no path specified")
}

  /** Save the document if it has changed */
  def saveOperation(): Unit =
      if (theSession.document.hasChanged)
        top.saveAs(theSession.path)
      else
        longFeedback("Unsaved")

  /**
   *  Cache for the file chooser: adapted to changing session path
   */
  object FileChooserCache {
    var _ppath: Path              = _
    var _fileChooser: FileChooser = _
    def chooser(): FileChooser = {
      if (_ppath!=theSession.parentPath) {
        _ppath       = theSession.parentPath
        _fileChooser = new FileChooser(_ppath.toFile) {
          fileSelectionMode = swing.FileChooser.SelectionMode.FilesOnly
          fileHidingEnabled = false
        }
      }
      _fileChooser
    }
  }
  /** A file chooser used by "Open" and "Save As" in the absence of parameter text */
  def fileChooser: FileChooser = FileChooserCache.chooser()

  def start(): Unit = { main(Array()); makeVisible() }

}

object UI extends Logging.Loggable
