package Red

import Red.UserInputHandlers.UserInputHandler

import scala.swing.{FileChooser, MenuBar}

/**
 *  An abstract view of a UI -- to enable some UI features to be defined outside the UI
 */
trait UIInterface {

  val theSession: EditSession

  val argLine, findLine, replLine: TextLine

  /**
   * A warning from Filter during the execution of  `body` is reported
   * only in the present UI.
   */
  def withFilterWarnings(title: String)(body: => Unit): Unit

   /**
   * Show a popup window, entitled `from`, with a warning message in it.
   */
  def warning(from: String, message: String): Unit

  /**
   * Place the given `message` in the feedback field, along
   * with details about the editing session.
   */
  def feedback(message: String): Unit
  /** ''Briefly'' pop-up a dialog with the given message */
  def flash(message: String): Unit
  def feedbackPersistently(message: String): Unit

  def feedbackWD(wd: String): Unit

  def longFeedback(msg: String): Unit


  /*
  def smallButton(label: String, toolTip: String = "")(act: => Unit): SmallButton

  def Button(label: String, toolTip: String = "")(act: => Unit): Button
   */

  /**
   *
   * Execute the given command, `c` under supervision of
   * the history manager, then present the feedback, and
   * finally make the session notify all handlers if
   * any changes have been made in the session. It is
   * this final method call that causes `theView` to
   * be synchronised with the session.
   */
  def UI_DO(c: Commands.Command[EditSession]): Unit

  /** The document view and the find and replace text lines all map Ctrl-F/Ctrl-R
   * to Find and Replace. This is the handler that implements the mapping.
   * It should be added to existing handlers for the view and the text lines.
   */
  val findreplHandler: UserInputHandler

  val theMenuBar: MenuBar

  def find(thePattern: String, backwards: Boolean): Unit

  def replace(thePattern: String, theReplacement: String, backwards: Boolean): Unit

  /** Has the document being edited here changed? */
  def hasChanged: Boolean

  /** Is this a genuine File-editing UI? It might be a cut ring UI,
   * which can dispense with parts of the "full" UI.
   */
  def isFileEditor: Boolean

  /** Close this UI (and only this) */
  def closeUI(): Unit

  /**
   * Is the top-level window of this GUI visible?
   * If it isn't then there might be a
   * performance advantage in avoiding
   * updating it. (See the prototype Cut Ring GUI
   * for an example).
   */
  def isVisible: Boolean

  /**
   * Force the top-level window of this GUI to
   * become visible.
   */
  def makeVisible(): Unit

  /**
   * Go to the location denoted by `location` as a `lineNumber x columnNumber` pair
   * with separator `,` or `:`, or `.`. If the separator and column number
   * are missing then take the column number as 0.
   */
  def goTo(location: String): Unit

  /** Notifies that this GUI and its associated
   * session have (already) been closed. Alias
   * for `top.sessionClosed`.
   */
  val sessionClosed: Notifier[String]
  /** Notifies requests
   * (from the `File` menu) to open files.
   */
  val openFileRequests: Notifier[String]

  /**
   * Force the top-level window to behave as if its
   * close button has been clicked. This will not actually
   * close the window or the session if there are
   * editing changes and the user elects to carry
   * on editing.
   */
  def close(): Unit

  /**
   * Open the file designated by the text on the argument line.
   * Prefixes such as `~/` and `+/` are treated in the usual way:
   * the former means the home directory, the latter means the
   * parent directory of the file being edited from this UI.
   */
  def openArglinePath(): Unit
  /** Save the document if it has changed */
  def saveOperation(): Unit

  /** A file chooser used by "Open" and "Save As" in the absence of parameter text */
  def fileChooser: FileChooser

  /** A directory chooser used by "Open" and "Save As" in the absence of parameter text */
  def dirChooser: FileChooser
  def start(): Unit
}
