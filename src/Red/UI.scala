package Red

import Commands._
import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import Red.UserInputHandlers._
import Red.Utils.relativeToHome

import java.awt.Color
import java.nio.file.{Files, Path, Paths}
import scala.swing.BorderPanel.Position._
import scala.swing.FileChooser.Result.{Approve, Cancel}
import scala.swing._

/** Graphical User Interface to an EditSession */
class UI(val theSession: EditSession) extends SimpleSwingApplication {
  import UI._
  import Utils.Menu
  import Menus.EmbeddedDynamicMenu
  /**
   * `theSession` emits feedback and warnings about things like find/replace failures
   * that we wish to report via this user interface.
   */
  locally {
    theSession.warnings.handleWith {
        case (from, message) => warning(from, message)
    }
  }

  locally {
    theSession.feedback.handleWith {
      case (from, message) => feedbackPersistently(s"$from: $message")
    }
  }


  /**
   * `Personalised.Bindings` emits feedback and warnings about various things
   * that we wish to report via this user interface.
   *
   */
  locally {
    Red.Personalised.Bindings.feedback.handleWith {
      case message => feedbackPersistently(message)
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
    if (theView==null) {
      Logging.Default.warn(s"$from: $message")
    } else
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
    focusable = false
  }

  private var delayFeedback = 0

  /**
   * Place the given `message` in the feedback field, along
   * with details about the editing session.
   */
  def feedback(message: String): Unit = {
    val (row, col) = theSession.getCursorPosition
    val changed = if (hasChanged) " (✖) " else " (✓) "
    val wd = if (theSession.CWD==Utils.homePath) "[~]" else if (theSession.CWD==theSession.parentPath) "[+]" else ""
    if (delayFeedback<=0)
      theFeedback.text =
      s"$wd $message ${theSession.displayPath}@${row+1}:$col $changed [${theSession.cursor}/${theSession.document.textLength}]"
    delayFeedback -= 1
    if (top != null) top.title = s"Red: ${theSession.displayPath} ${changed}"
  }

  /** ''Briefly'' pop-up a dialog with the given message */
  def flash(message: String): Unit = {
    if (theFeedback==null || !theFeedback.visible)
      Logging.Default.warn(s"Flash: $message")
    else {
      theFeedback.text = message
    }
  }

  def feedbackPersistently(message: String): Unit = {
    delayFeedback = 2
    flash(message)
  }

  def feedbackWD(wd: String) = {
    theFeedback.text = s"CWD: $wd"
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
    delayFeedback=0
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

  class  SmallButton(val label: String, toolTip: String="", act: => Unit) extends Button() {
      action = new Action(label) { def apply(): Unit = act }
      font = Utils.smallButtonFont
      focusable = false
      def setLabel(newLabel: String): Unit = {
        val metrics = peer.getFontMetrics(font)
        val labWidth = metrics.stringWidth("MMMMMM")
        val charHeight = metrics.getHeight
        preferredSize = new Dimension(labWidth+4, charHeight+2)
        action.text = newLabel
      }
      if (toolTip.nonEmpty) tooltip=toolTip
      setLabel(label)
  }

  def smallButton(label: String, toolTip: String="")(act: => Unit): SmallButton = new SmallButton(label, toolTip, act)

  def Button(label: String, toolTip: String="")(act: => Unit): Button = new Button(new Action(label) { def apply(): Unit = act } ) {
    font = Utils.menuButtonFont
    focusable = false
    if (false) {
      val metrics = peer.getFontMetrics(font)
      val labWidth = metrics.stringWidth(label)
      val charHeight = metrics.getHeight
      preferredSize = new Dimension(labWidth, charHeight)
    }
    focusable = false // Magic to avoid blue focus ring
    if (toolTip.nonEmpty) tooltip=toolTip
  }


  /** The history manager for `theSession`. It responds to DO/UNDO
   *  commands as described in its specification
   */
  private val history = new Command.StateChangeHistory(theSession)
  /** An undo button */
  private val undoButton = smallButton(/*"\u2770"*/"<", toolTip="Undo last edit") { UI_DO(history.UNDO) } //
  /** A redo button */
  private val redoButton = smallButton(/*"\u2771"*/">", toolTip="Redo last undone edit") { UI_DO(history.REDO) } //

  locally {
    history.handleWith {
      case (done, undone) =>
        undoButton.setLabel(if (done>0) s"$done<" else " < ")
        undoButton.enabled = done > 0
        redoButton.enabled = undone > 0
        redoButton.setLabel(if (undone>0) s"$undone>" else  "> ")
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

    case Instruction(Key.F8, _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)

    case Instruction(Key.R,  _, mods) => replace(findLine.text, replLine.text, backwards = mods.hasShift)

    case Instruction(Key.Decimal, _, mods) if (mods.hasControl) =>
      replLine.requestFocusInWindow()
      replLine.text=""

    case Instruction(Key.Decimal, _, mods) =>
      replace(findLine.text, replLine.text, backwards = mods.hasShift)

    case Instruction(Key.Numpad0, _, mods) if (mods.hasControl) =>
      findLine.requestFocusInWindow()
      findLine.text = ""

    case Instruction(Key.Enter, _, mods) if (mods.hasControl) =>
      argLine.requestFocusInWindow()
      argLine.text = ""

    case Instruction(Key.Numpad0, _, mods)  =>
      if (mods.hasAlt && theSession.hasSelection) {
        findLine.text = theSession.selectionText()
        regexCheck.selected=false
      }
      find(findLine.text, backwards = mods.hasShift)

    case Instruction(Key.F7, _, mods)  =>
      if (mods.hasAlt && theSession.hasSelection) {
        findLine.text = theSession.selectionText()
        regexCheck.selected=false
      }
      find(findLine.text, backwards = mods.hasShift)

    case Instruction(Key.F, _, mods)  =>
      if (mods.hasAlt && theSession.hasSelection) {
        findLine.text = theSession.selectionText()
        regexCheck.selected=false
      }
      find(findLine.text, backwards = mods.hasShift)


    case Diacritical(mark: Char) => feedback(s"[$mark]")

  }

  private val argLine: TextLine = new TextLine(25) {
    focusable = true
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

      case Diacritical(mark: Char) => feedback(s"[$mark]")
    }
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()

    tooltip = "Argument(s) for some commands"
  }

  private val findLine: TextLine = new TextLine(25) {
    focusable = true
    override def firstHandler: UserInputHandler = findreplHandler
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()
  }

  private val replLine: TextLine = new TextLine(25) {
    focusable = true
    override def firstHandler: UserInputHandler = findreplHandler
    /** hand back focus to the main text */
    override def mouseExited(): Unit = theView.requestFocusInWindow()
  }

  private val regexCheck: CheckBox = new CheckBox("") {
    focusable = false
    tooltip  = "When enabled, treat \u24bb pattern as regular expression, and \u24c7 as a regular expression template" // (F)
  }

  /**
   *  The top line below the menu bar holds the find and replace
   *  buttons and text lines, as well as an undo and a redo button
   *  linked to the undo history
   */
  private val theWidgets = new BoxPanel(Orientation.Horizontal) {
    contents += Button("\u24b6", toolTip = "(Clear) the \u24b6 field \u2191") {
      argLine.text = ""
      argLine.requestFocusInWindow()
      // TODO: make this button drop down a menu of recents
    } // (A)
    contents += argLine
    contents += regexCheck
    contents += Button("\u24bb", toolTip = "Clear the adjacent find pattern") {
      findLine.text = ""
      findLine.requestFocusInWindow()
      // TODO: make this button drop down a menu of recents
    } // (F)
    contents += findLine
    contents += Button("\u24c7", toolTip = "Clear the adjacent replacement template") {
      replLine.text = ""
      replLine.requestFocusInWindow()
      // TODO: make this button drop down a menu of recents
    } // (R)
    contents += replLine
  }



  private val theMenuBar: MenuBar = new MenuBar {
    import Buttons.menuButton
    font = Utils.menuFont



    contents += new Utils.Menu("Red") {

      contents += new Menus.DynamicMenu("cd ") {
        font = Red.Utils.menuButtonFont

        def theLabel():  Component  = {
          val cwd = if (theSession.CWD==theSession.parentPath) "+" else relativeToHome(theSession.CWD)
          new Buttons.CentredLabel(s"  CWD: ${cwd}  ")  { font = Utils.menuButtonFont; background = Color.lightGray }
        }

        def theParent(): Component  = new Buttons.CentredLabel(s"  +: ${relativeToHome(theSession.parentPath)}  ") {
          font = Utils.menuButtonFont
          background = Color.lightGray
        }

        def selectTheParent(): Component = menuButton(s"cd +: ${relativeToHome(theSession.parentPath)} ", toolTip = s"Change working directory to ${relativeToHome(theSession.parentPath)}") {
          theSession.CWD = theSession.parentPath;
          feedbackWD(theSession.CWD.toString)
        }

        val homeDir = menuButton("cd ~", toolTip = "Change working directory to user's home directory") {
          theSession.CWD = Utils.homePath;
          feedbackWD(theSession.CWD.toString)
        }

        val chooseDir = menuButton("cd \u24b6", toolTip = "Change working directory using dialogue or nonempty \u24b6 field") {
          var text = argLine.text.trim
          if (text.isEmpty) {
            val chooser = dirChooser
            chooser.showOpenDialog(top) match {
              case Approve => text = chooser.selectedFile.toString
              case Cancel  => text = ""
            }
          }
          if (text.nonEmpty) theSession.CWD = Utils.toPath(text)
          feedbackWD(theSession.CWD.toString)
        }

        def dynamicContents: Seq[Component] = List ( theLabel(), Separator(), selectTheParent(), homeDir, chooseDir)
      }

      contents += Separator()

      contents += new Buttons.PersistentCheckItem("Typeover", "typeover", theSession.typeOverSelection_=(_), Personalised.Settings.typeOverSelection) {
        tooltip  = "When this is enabled, the selection is automatically cut when material is typed"
        font     = Utils.menuButtonFont
      }

      contents += new Buttons.PersistentCheckItem("Select {...}", "autoselect", theSession.clickSelects_=(_), Personalised.Settings.clickSelects) {
        tooltip  = "When this enabled, a mouse-click adjacent to bracketed material of any kind selects that material"
        font     = Utils.menuButtonFont
      }


      contents += new Buttons.PersistentCheckItem("Auto indent", "autoindent", theSession.autoIndenting=(_), Personalised.Settings.autoIndenting) {
        tooltip  = "When this is enabled, the insertion of a newline will align the cursor (and any non-space material to its right) with the indentation of the current line"
        font     = Utils.menuButtonFont
      }

      contents += new Buttons.PersistentCheckItem("Show find cost", "showsteps", { b => Utils.showSteps=b }, Utils.showSteps) {
        tooltip  = "When this is enabled, a successful find reports the the cost of the succeeding match as a proportion of the prevailing limit"
        font     = Utils.menuButtonFont
      }

      if (theSession.hasCutRing) {
        contents += Separator()
        contents += menuButton("Cut Ring", toolTip = "Show the cut-ring control window") {
          CutRingUI.refreshIfVisible()
        }
      }

      contents += Separator()

      contents += Features.menu

      contents += Separator()
      contents += Separator()

      contents += menuButton("Quit", toolTip = "Quit now if there are no unsaved document sessions; else ask each unsaved document session what to do")  { top.closeOperation() }

    }

    contents += new Utils.Menu("File") {

      contents += menuButton("Open \u24b6", "Edit the document at the path specified by the \u24b6 field or by making a choice of path") {
        openArglinePath()
      }

      contents += Utils.Recents.menu()

      contents += menuButton("Open New", "Edit the document at the path specified by the \u24b6 field or by making a choice of path") {
        Red.Server.process(Utils.freshDocumentName())
      }

      contents += Separator()
      contents += Separator()

      contents += menuButton("Save") {
        saveOperation()
      }

      contents += menuButton("Save as \u24b6", "Save at the path specified by the \u24b6 field or by making a choice of path") {
        val text = argLine.text.trim
        if (text.isEmpty)
          { val chooser = fileChooser
            chooser.showSaveDialog(top) match {
              case Cancel  =>
                feedbackPersistently("Save as: no path specified")
              case Approve =>
                val path = chooser.selectedFile.getAbsolutePath
                top.saveAs(path.toString)
            }
          }
        else
        top.saveAs(Utils.localizePath(text, theSession.CWD, Utils.toParentPath(theSession.path)))
      }

      contents += menuButton("Save & Quit", "Save the document if it needs saving; then close this session.") {
        close()
      }

    } // File Menu

    /** A group that generates menu items for managing formatting options  */
    val formatting = new Buttons.Group() {
      def select(value: String): Unit = {
        if (value == "")
          UI_DO(EditSessionCommands.formatter(argLine.text, "fmt"))
        else
          withFilterWarnings("fmt ") {
            UI_DO(EditSessionCommands.formatter(argLine.text, s"redformat '$value'", List(value)))
          }
      }
    }

    contents += new Utils.Menu("Edit") {

          contents += menuButton("Format ...", "Format the selection using the current mode") {
             withFilterWarnings("format") { formatting.select() }
          }

          contents += Separator()

          contents += menuButton("ABC -> abc", "Make the selection lowercase", centred = true) {
            UI_DO(EditSessionCommands.lowerCaseFilter)
          }

          contents += menuButton("abc -> ABC", "Make the selection uppercase", centred = true) {
            UI_DO(EditSessionCommands.upperCaseFilter)
          }

        contents += Separator()

        contents += menuButton("\u24bb -> \u24c7", "Replace \u24bb with \u24c7 throughout the selection, using the current find/replace mode", centred = true) {
          val asRegex = regexCheck.selected
          UI_DO(EditSessionCommands.replaceAllInSelection(findLine.text, replLine.text, asRegex))
        }

        contents += Separator()

        contents += new Utils.Menu(" Formats ") {
          font = Utils.menuButtonFont
          contents += formatting.CheckBox(" ...", "", "Format the selection in non-prefix mode, and set mode to non-prefix")
          contents += formatting.CheckBox(" -p * ...", "*", "Format the selection with redformat -p * and set mode to -p *")
          contents += formatting.CheckBox(" -p | ...", "|", "Format the selection with redformat -p | and set mode to -p |")
          contents += formatting.CheckBox(" -p # ...", "#", "Format the selection with redformat -p # and set mode to -p #")
          contents += formatting.CheckBox(" -p -- ...", "--", "Format the selection with redformat -p -- and set mode to -p --")
        }

    } // Edit Menu

    contents += new Utils.Menu("Pipe") {
      // should piped output replace the selection or prefix it
      var augmentSelection: Boolean = false

      // Pipe the selection through ...
      contents += menuButton("\u24b6  < ...", "Pipe the selection through the shell command \u24b6 (see also \"Append Selection\")") {
        withFilterWarnings("\u24b6  < ...") { UI_DO(EditSessionCommands.pipeThrough(argLine.text, replaceSelection = !augmentSelection)) }
      }

      for { program <- Personalised.pipeNames } {
        contents += menuButton(s"$program < ...", s"Pipe the selection through the shell command \"$program\" (see also \"Append Selection\")") {
          withFilterWarnings(s"$program < ...") { UI_DO(EditSessionCommands.pipeThrough(program, replaceSelection = !augmentSelection)) }
        }
      }

      contents += Separator()

      contents += new CheckMenuItem("Append selection") {
        tooltip  = "When enabled, the original selection is appended to the piped output from the above commands."
        font     = Utils.buttonFont
        selected = augmentSelection
        listenTo(this)
        reactions += {
          case event.ButtonClicked(_) =>
            augmentSelection = selected
        }
      }

    } // Pipe Menu

    contents += new Label("        ")

    contents += Button("Tex", toolTip = "Run redpdf now") {
      saveOperation()
      UI_DO(EditSessionCommands.latexToPDF)
    }

    contents += new EmbeddedDynamicMenu("\\begin{...}", { Red.Personalised.latexBlockTypes }) {
      font = Utils.menuButtonFont
      prefix += menuButton("%%%%%%%%") {
        val header =
          """%%%%%%%%%%%%%%%%%%%%%%%%
            |%%%%%%%%%%%%%%%%%%%%%%%%
            |%%%%%%%%%%%%%%%%%%%%%%%%
            |""".stripMargin
        UI_DO(EditSessionCommands.latexInsert(header))
      }

      prefix += Separator()

      def component (block: String): Component = {
        if (block == "-")
          Separator()
        else
          menuButton(s"""\\begin{$block}""") { UI_DO(EditSessionCommands.latexBlock(block)) }
      }

      // contents ++= dynamic

      suffix += Separator()
      suffix += menuButton("\\begin{\u24b6}", "Embed selection in latex block named in \u24b6") {
        UI_DO(EditSessionCommands.latexBlock(argLine.text.trim))
      }

      suffix += menuButton("""\begin{...}->...""", "Extract content of selected latex block") {
        UI_DO(EditSessionCommands.latexUnblock)
      }

      suffix += Separator()

      // Infrequent additions
      suffix += new Menu("Class") {


        contents += menuButton("\\documentclass{article}") {
          val up = "\\usepackage[]{}"
          val header =
            s"""\\documentclass[11pt,a4paper]{article}
               |%%%%%%%%%%%%%%%%%%%%%
               |$up
               |%%%%%%%%%%%%%%%%%%%%%
               |\\author{}
               |\\title{}
               |\\date{}
               |%%%%%%%%%%%%%%%%%%%%%
               |\\begin{document}
               |\\maketitle
               |
               |\\end{document}
               |""".stripMargin
          UI_DO(EditSessionCommands.latexInsert(header))
        }

        contents += menuButton("\\documentclass{letter}") {
          val header =
            s"""\\documentclass[12pt,lab|wor|home|magd,bernard|sufrin]{letter} %
               |\\To{lines\\\\of\\\\mailing address}
               |\\Dear[Dear]{Victim}
               |\\Re{subject matter}
               |    This is the body
               |\\Ps{ps paragraph}
               |\\PostSig{post signature para}
               |\\Cc{carrbon1, carbon2, ...}
               |\\Sign[yours sincerely]{Bernard Sufrin}
               |""".stripMargin
          UI_DO(EditSessionCommands.latexInsert(header))
        }

        contents += Separator()
        contents += Separator()

        contents += menuButton("Tex source := \u24b6", toolTip = "Change default tex source using dialogue or nonempty \u24b6 field") {
          var text = argLine.text.trim
          if (text.isEmpty) {
            val chooser = fileChooser
            chooser.showOpenDialog(top) match {
              case Approve => text = chooser.selectedFile.toString
              case Cancel  => text = ""
            }
          }
          if (text.nonEmpty) theSession.TEX=Utils.toPath(text)
          feedbackPersistently(s"Tex source: ${theSession.TEX.toString}")
        }

        contents += menuButton(s"Default tex source := ${theSession.path}", toolTip = "Change default latex source to current file") {
          theSession.TEX=Utils.toPath(theSession.path)
          feedbackPersistently(s"Tex source: ${theSession.TEX.toString}")
        }
      }

    }

    contents += Glue.horizontal()

    contents += Button("Pandoc", toolTip = "Run redpandoc now") {
      saveOperation()
      UI_DO(EditSessionCommands.pandocToPDF)
    }

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
    UI_DO(EditSessionCommands.find(thePattern, backwards, asRegex){ theView.requestFocusInWindow() })
  }

  def replace(thePattern: String, theReplacement: String, backwards: Boolean): Unit = {
    val asRegex = regexCheck.selected
    UI_DO(EditSessionCommands.replace(thePattern, theReplacement, backwards, asRegex))
  }

  val top: ControllingFrame = new ControllingFrame(theSession) {
    title = s"Red: ${theSession.path}"
    background = Color.lightGray
    contents = thePanel
    if (isFileEditor) menuBar = theMenuBar

    val indentKeys: UserInputHandler = {
      case Instruction(Key.Tab, _, Alt)       =>
        UI_DO(EditSessionCommands.indentSelectionBy(argLine.text))
      case Instruction(Key.Tab, _, AltShift)  =>
        UI_DO(EditSessionCommands.undentSelectionBy(argLine.text))
    }

    theView.keystrokeInput.handleWith {
        indentKeys        orElse
        handlers.mouse    orElse
        findreplHandler   orElse
        handlers.keyboard orElse {
        case Instruction(Key.P, _, Control) =>
             UI_DO(EditSessionCommands.selectParagraph &&& EditSessionCommands.formatter(argLine.text, "fmt"))
        case Instruction(Key.Z, _, ControlShift) => UI_DO(history.REDO)
        case Instruction(Key.Z, _, Control) => UI_DO(history.UNDO)
        case Instruction(Key.Q, _, Control) =>
          top.closeOperation()
        case Instruction(Key.S, _, Control) =>
          saveOperation()
        case Diacritical(mark: Char) =>
          feedback(s"[$mark]")

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
  if (path.nonEmpty) openFileRequests.notify(path) else feedbackPersistently("Open: no path specified")
}

  /** Save the document if it has changed */
  def saveOperation(): Unit =
      if (theSession.document.hasChanged)
        top.saveAs(theSession.path)
      else
        longFeedback("Unsaved")

  /**
   *  Cache for the file chooser: adapted to changing session path, and
   *  modality of the selection.
   */
  object FileChooserCache {
    var _ppath: Path              = _
    var _fileChooser: FileChooser = _
    def chooser(dirsOnly: Boolean = false): FileChooser = {
      if (_ppath!=theSession.parentPath) {
        _ppath       = theSession.parentPath
        _fileChooser = new FileChooser(_ppath.toFile) {
          fileSelectionMode = swing.FileChooser.SelectionMode.FilesOnly
          fileHidingEnabled = false
        }
      }
      _fileChooser.fileSelectionMode =
        if (dirsOnly)
          swing.FileChooser.SelectionMode.DirectoriesOnly
        else
          swing.FileChooser.SelectionMode.FilesOnly
      _fileChooser
    }
  }
  /** A file chooser used by "Open" and "Save As" in the absence of parameter text */
  def fileChooser: FileChooser = FileChooserCache.chooser(dirsOnly=false)
  /** A directory chooser used by "Open" and "Save As" in the absence of parameter text */
  def dirChooser: FileChooser = FileChooserCache.chooser(dirsOnly=true)

  def start(): Unit =
  { if (logging) finer(s"Starting UI with window $top")
    main(Array())
    if (logging) finer(s"Making visible UI with window $top")
    makeVisible()
  }

}

object UI extends Logging.Loggable
