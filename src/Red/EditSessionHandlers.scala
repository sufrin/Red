package Red

import Red.EditSessionCommands.insertCommand
import Red.Personalised.Bindings.RedScriptEvaluator.EditSessionCommand
import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import RedScript.Language.{Nothing, SExp}

import scala.collection.mutable.ListBuffer



/** Effectively a mapping from userinput events to their meanings
 * as executable `SExp`s.
 * == NB: ==
 * Such a meaning is (usually) a (boxed-for-RedScript) `SessionCommand` that is
 * to be invoked (by `DO`) within the `RedScriptInputHandler` (see below).
 *
 * This will first (or high) in the pecking order of handlers for events generated by the
 * `DocumentView` component of the user-interface to an editing session.
 */
trait EventMap {
  def apply(input: UserInput): SExp
  def mapTo(input: UserInput, sexp: SExp): Unit
  def clear(): Unit
  def includesMap(other: EventMap): Unit
}

object EventMap {
  def apply(): EventMap = new EventMap {
    override def toString: String          = s"EventMap(${meaning.mkString("", " ", "")})"
    def clear():Unit                       = meaning.clear()
    def mapTo(inputEvent: UserInput, sexp: SExp): Unit = meaning.addOne(inputEvent, sexp)

    private val inclusions = new ListBuffer[EventMap]()

    def apply(inputEvent: UserInput): SExp =
      meaning.get(inputEvent) match {
        case Some(sexp) => sexp
        case None =>
          var result: SExp = Nothing
          for { map <- inclusions if result==Nothing } result = map(inputEvent)
          result
       }

    def includesMap(other: EventMap): Unit =
      inclusions += other
    }

    private val meaning: collection.mutable.HashMap[UserInput, SExp] = new collection.mutable.HashMap[UserInput, SExp]

}


/**
 *
 * @param DO run a command
 * @param session *normally* the editsession that is being driven by whatever implements DO
 * @param eventMap a map from input events to the (executable) `SExp`ressions they denote. These
 *                 are *normally* `EditSession` commands. **Only** the `redScriptInputHandler` uses
 *                 this mapping. Such mappings are built during the personalised configuration
 *                 of Red.
 */
case class EditSessionContext(DO: Commands.Command[EditSession]=>Unit, session: EditSession, eventMap: EventMap)

/**
 *  This class defines handlers for keyboard and mouse events.
 *  The composite `keyboard` and `mouse` handlers defined here
 *  invoke appropriate `Command`s defined by `EditSessionCommands` with `UI_DO`.
 *
 *  The handler `singleLineKeyboard` responds the same as `keyboard` except
 *  for `Key.Up`, `Key.Down`, and `'\n'`, to which it responds by doing nothing.
 *
 *  `UI_DO` should execute its argument `command`, by invoking `command.UI_DO` on
 *  a suitable target editing session. This may be done directly, or
 *  through the intermediary of a `StateChangeHistory` manager
 *  that provides additional functionality.
 *
 *  There is an implied relationship between the
 *  source of the user input events and the target
 *  of the commands, namely that the events are sourced
 *  by a component viewing a document associated with
 *  the target.  But we do not exclude the possibility
 *  of "virtual" input events being generated by low-level
 *  testing frameworks.
 */
class EditSessionHandlers(context: EditSessionContext) {
  
  private val commands = EditSessionCommands
  private val UI_DO:     Commands.Command[EditSession]=>Unit = context.DO
  private val eventMap:  EventMap                            = context.eventMap

  type UserInputHandler = Notifier.Handler[UserInput]

  case object UNDEFINED extends Throwable

  /**
   * This handler is expected to be close to the top of the handler pecking order
   * for the event-generating view component. It handles "customised"
   * behaviour very efficiently, using commands ''specified'' by an `EventMap`
   * built *during configuration*.
   *
   * Characters not associated with a command are treated as `insertCommand`s. Use
   * `singleLineRedScriptInputHandler` for the single-line components used for the
   * (A), (F), and (R) fields.
   *
   * Instructions not associated with a command are *refused* here: this means that
   * they will be seen by any handlers *below* this handler in the pecking order
   * of the view that generates the events -- in short, they'll evoke the
   * *built-in* behaviour programmed into Red.
   *
   */
  val redScriptInputHandler: UserInputHandler = new UserInputHandler {

        override def isDefinedAt(input: UserInput): Boolean = {
          try   { handleInput(input); true }
          catch {
            case UNDEFINED => false
          }
        }

        def apply(input: UserInput): Unit = {}

        def handleInput(input: UserInput): Unit = {
          input match {
            case i: Instruction =>
              eventMap(i) match {
                case EditSessionCommand(name, command) =>
                  UI_DO(command)
                case other =>
                  throw UNDEFINED
              }

            case c: Character   =>
               eventMap(c) match {
                 case EditSessionCommand(name, command) =>
                   UI_DO(command)
                 case Nothing =>
                   UI_DO(insertCommand(c.char))
               }

            case other => throw UNDEFINED
          }
        }
      }

      /**  The default keyboard input handler.
       *
       *   This is the keyboard used in the uncustomised editor.
       *   It was originally the only keyboard input handler.
       */
      val keyboard: UserInputHandler =  {
        case Character(char, _, NoModifier)            => UI_DO(commands.insertCommand(char))
        case Character(char, _, Shift)                 => UI_DO(commands.insertCommand(char))
        case Character(char, _, mods) if mods.hasAlt   => UI_DO(commands.insertCommand(char))

        case Instruction(Key.Tab, _, NoModifier)       => UI_DO(commands.indentOrTab)
        case Instruction(Key.Tab, _, Shift)            => UI_DO(commands.undentSelection)

        case Instruction(Key.BackSpace, _, NoModifier) => UI_DO(commands.delete)
        case Instruction(Key.BackSpace, _, Control)    => UI_DO(commands.flip)

        case Instruction(Key.X, _, Control)       => UI_DO(commands.cut)
        case Instruction(Key.C, _, Control)       => UI_DO(commands.copy)
        case Instruction(Key.V, _, Control)       => UI_DO(commands.paste)
        case Instruction(Key.B, _, Control)       => UI_DO(commands.exchangeCut)

        // Sufrin's favourites
        case Instruction(Key.F1, _, NoModifier)       => UI_DO(commands.cut)
        case Instruction(Key.F3, _, NoModifier)       => UI_DO(commands.copy)
        case Instruction(Key.F2, _, NoModifier)       => UI_DO(commands.paste)
        case Instruction(Key.F4, _, NoModifier)       => UI_DO(commands.exchangeCut)
        case Instruction(Key.F5, _, NoModifier)       => UI_DO(commands.exchangeMark)

        case Instruction(Key.Home, _, NoModifier) => UI_DO(commands.toHome)
        case Instruction(Key.End, _, NoModifier)  => UI_DO(commands.toEnd)
        case Instruction(Key.A, _, Control)       => UI_DO(commands.selectAll)

        case Instruction(Key.Left, _, NoModifier)  => UI_DO(commands.prevChar)
        case Instruction(Key.Right, _, NoModifier) => UI_DO(commands.nextChar)
        case Instruction(Key.Down, _, NoModifier)  => UI_DO(commands.nextLine)
        case Instruction(Key.Up, _, NoModifier)    => UI_DO(commands.prevLine)

        case Instruction(Key.PageUp,   _, NoModifier) => UI_DO(commands.selectAnyBraUpwards)
        case Instruction(Key.PageUp,   _, Control)    => UI_DO(commands.selectMatchingUp)
        case Instruction(Key.PageDown, _, NoModifier) => UI_DO(commands.selectAnyKetDownwards)
        case Instruction(Key.PageDown, _, Control)    => UI_DO(commands.selectMatchingDown)

        case Instruction(Key.Escape, _, NoModifier) => UI_DO(commands.abbreviate)
        case Instruction(Key.Escape, _, AltShift)   => UI_DO(commands.unicode)
      }


      /** A handler that responds to single mouse clicks, but ignores
       *  multiple mouse clicks and mouse released events.
       *
       *  Selection-by-dragging (the cursor) is implemented. When the
       *  cursor button is pressed, the mark and the cursor are moved to
       *  the mouse position, and the session is now in a state where
       *  dragging the cursor button extends the selection to the  mouse
       *  position.
       *
       *  If the cursor button is pressed or dragged with the control modifier this just
       *  moves the cursor.
       *
       */
      val mouse: UserInputHandler =  {
        case MousePressed(row, col, 1, Button1)           =>
             UI_DO(EditSessionCommands.setCursorAndMark(row, col) &&& EditSessionCommands.selectMatching.when(_.clickSelects))

        case MousePressed(row, col, n, Button1)  if 2<=n  => UI_DO(commands.selectChunk(row, col, n))
        case MouseDragged(row, col,    Button1)           => UI_DO(commands.dragCursor(row, col))
        case MouseReleased(_, _,       Button1)           => UI_DO(commands.mouseUp)

        case MousePressed(row, col, n, ControlButton1)  => UI_DO(commands.setCursor(row, col))
        case MouseDragged(row, col,    ControlButton1)  => UI_DO(commands.dragCursor(row, col))
        case MouseReleased(_, _,       ControlButton1)  => UI_DO(commands.mouseUp)

        case MousePressed(row, col, n, detail) if detail.hasButton3 => UI_DO(commands.setMark(row, col))

        // Multiple presses
        case MousePressed(_, _, _, Button1)  => ()
        case MousePressed(_, _, _, Button3)  => ()
        case MouseReleased(_, _,   Button3)  => ()
      }

      /**
       *   A handler that does nothing in response to `Key.Up`, `Key.Down`, and `'\n'`;
       *   and treats tab as space
       */
      private val ignoreMultiLineKeys: UserInputHandler = {
        case Character('\n', _, _)       => ()
        case Instruction(Key.Down, _, _) => ()
        case Instruction(Key.Up, _, _)   => ()
        case Instruction(Key.Tab, _, _)  => UI_DO(commands.insertCommand(' '))
      }

      /**
       *  A handler that does nothing in response to `Key.Up`, `Key.Down`, and `'\n'` but otherwise responds to
       *  keys in the same way as  `keyboard`
       */
      val singleLineKeyboard: UserInputHandler  = ignoreMultiLineKeys orElse keyboard

      /**
       *  A handler that does nothing in response to `Key.Up`, `Key.Down`, and `'\n'` but otherwise responds to
       *  keys in the same way as  `redScriptInputHandler`
       */
      val singleLineRedScriptInputHandler: UserInputHandler = ignoreMultiLineKeys orElse redScriptInputHandler

      /**
       *
       */
       def unhandled(caption: String): UserInputHandler = { case input: UserInput => UI_DO(EditSessionCommands.unhandledInput(caption)(input)) }

}
