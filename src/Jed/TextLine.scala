package Jed
import  Red._

import UserInputHandlers._
import scala.swing._

/**
 * A single, horizontal, line of text with configurable keyboard and
 * mouse behaviour. Input events are handled by the composition:
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
 * Newline characters are represented visually and internally by the character sequence specified as `surrogateLF`,
 * which by default is `"(ɴ)"` (ɴ is at unicode \u0274).
 */
class TextLine(cols: Int) extends BoxPanel(Orientation.Horizontal) {
  import TextLine._

  protected val (realLF, surrogateLF): (String, String) = ("\n", "(\u0274)")

  protected val doc:     DocumentInterface      = new Document()
  protected val session: EditSession            = new EditSession(doc, "") {
     override def insert(string: String): Unit  = super.insert(string.replace(realLF, surrogateLF))
     override def selectionText(): String       = super.selectionText().replace(realLF, surrogateLF)
  }
  protected val view     = new DocumentView(session, 1, cols, font=Settings.widgetFont)

  def DoHere(command: Commands.Command[EditSession]): Unit = { command.DO(session); session.notifyHandlers() }

  protected val handler = new EditSessionHandlers(DoHere)

  protected def firstHandler: UserInputHandler = { case _: UserInput if false => {} }
  protected def lastHandler:  UserInputHandler = { case other: UserInput => Logging.Default.warn(s"TextLine: unhandled [[$other]]") }

  locally {
    contents += view
    view.viewLineNumbers(0)
    view.keystrokeInput.handleWith {
        firstHandler                  orElse
        handler.singleLineKeyboard    orElse
        handler.mouse                 orElse
        lastHandler
    }
  }

  def text: String                  = doc.toString.replace(surrogateLF, realLF)
  def text_=(newText: String): Unit = {
    session.cutAll()
    session.insert(newText)
    session.cursor=newText.length
    session.selection=Jed.NoSelection
    session.notifyHandlers()
  }
}

object TextLine extends Logging.Loggable
