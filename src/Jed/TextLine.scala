package Jed
import Jed.EditSessionCommands.{SessionCommand, StateChangeOption}
import Red.UserInputHandlers._
import Red._

import scala.swing._

/**
 * A single, horizontal, line of text with configurable keyboard and
 * mouse behaviour. Newline (`'\n'`) characters are represented visually and
 * by the character sequence specified as `surrogateLF`,
 * which by default is `"(ɴ)"` (ɴ is at unicode \u0274). They
 * are externalized as `'\n'`.
 *
 * Input events are handled by the composition:
 * {{{
 *   firstHandler orElse handleOneLineInput orElse lastHandler
 * }}}
 * The first, and last handlers are initially defined to fail; but may be
 * overridden in subclasses so as to provide more refined overall behaviour
 * for subclasses that need it.
 *
 * For example, here is a `TextLine` that responds
 * in the usual way to every input event, except for `'\n'`, to which it responds
 * by invoking `find` on the current text, and for the standard `Paste` key (`control-V`),
 * to which it responds by doing nothing.
 * {{{
 *  val finder = new Jed.TextLine(25) {
 *      override def firstHandler: UserInputHandler = {
 *        case Character('\n', _, 0) => find(text)
 *        case Instruction(Key.V, _, Control) => ()
 *      }
 * }
 * }}}
 *
 */
class TextLine(cols: Int) extends BoxPanel(Orientation.Horizontal) {

  protected val (realLF, surrogateLF): (String, String) = ("\n", "(\u0274)")

  private val insertLF: SessionCommand  = new SessionCommand {
    def DO(session: EditSession): StateChangeOption = {
      session.insert(surrogateLF)
      None
    }
  }

  private trait LFPlugin extends EditSession { host =>
    override def insert(string: String): Unit  = super.insert(string.replace(realLF, surrogateLF))
    override def insert(ch: Char): Unit        = super.insert(s"$ch")
    override def selectionText(): String       = super.selectionText().replace(realLF, surrogateLF)
  }

  protected val doc:     DocumentInterface      = new Document()
  protected val session: EditSession            = new EditSession(doc, "") with LFPlugin
  protected val view     = new DocumentView(session, 1, cols, font=Utils.widgetFont) {
    override def mouseExited(): Unit = TextLine.this.mouseExited()
  }

  def mouseExited(): Unit = {}

  def DO(command: Commands.Command[EditSession]): Unit = { command.DO(session); session.notifyHandlers() }

  protected val handler = new EditSessionHandlers(DO)

  protected val lfHandler: UserInputHandler = {
    case Character('\n', _, _) => DO(insertLF)
  }

  protected def firstHandler: UserInputHandler = { case _: UserInput if false => {} }
  protected def lastHandler:  UserInputHandler = { case other: UserInput => Logging.Default.warn(s"TextLine: unhandled [[$other]]") }

  locally {
    contents += view
    view.viewLineNumbers(0)
    view.keystrokeInput.handleWith {
        firstHandler                  orElse
        lfHandler                     orElse
        handler.singleLineKeyboard    orElse
        handler.mouse                 orElse
        lastHandler
    }
  }

  /** The current text in the line */
  def text: String                  = doc.toString.replace(surrogateLF, realLF)

  /** Set the current text in the line */
  def text_=(newText: String): Unit = {
    session.cutAll()
    session.insert(newText)
    session.cursor=newText.length
    session.selection=Jed.NoSelection
    session.notifyHandlers()
  }
}

object TextLine extends Logging.Loggable
