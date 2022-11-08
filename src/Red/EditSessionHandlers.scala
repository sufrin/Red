package Red

import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import RedScript.SourcePosition

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
class EditSessionHandlers(val UI_DO: Commands.Command[EditSession]=>Unit) {
  
  private val commands = EditSessionCommands
  
  type UserInputHandler = Notifier.Handler[UserInput]

  case object UNDEFINED extends Throwable

  val redScriptInputHandler: UserInputHandler = new UserInputHandler {
        val canHandleInput = RedScript.Language.Variable("canHandleInput")
        val handleInput = RedScript.Language.Variable("handleInput")
        handleInput.position = SourcePosition("redScriptInputHandler")
        canHandleInput.position = handleInput.position

        override def isDefinedAt(input: UserInput): Boolean = {
          try   { handleInput(input); true }
          catch {
            case UNDEFINED => false
          }
        }

        def apply(input: UserInput): Unit = {}

        def handleInput(input: UserInput): Unit = {
          /*
          val script = RedScript.Language.SExps(List(handleInput, Personalised.Bindings.RedScriptEvaluator.UserInput(input)))
          script.position = handleInput.position
            Personalised.Bindings.RedScriptEvaluator.run(script) match {
              case RedScript.Language.Str(commandName) =>
                CommandsDict(commandName) match {
                  case Some(command) => UI_DO(command)
                  case None          => throw UNDEFINED
                }
              case Personalised.Bindings.RedScriptEvaluator.SessionCommand(command) =>
                   UI_DO(command)
              case RedScript.Language.SExps(Nil) =>
                   throw UNDEFINED
            }
            */

          input match {
            case i: Instruction =>
              Personalised.Bindings.RedScriptEvaluator.instruction.get(i) match {
                case Some(Personalised.Bindings.RedScriptEvaluator.EditSessionCommand(name, command)) =>
                  UI_DO(command)
                case other =>
                  println(s"Not handling $i")
                  println(Personalised.Bindings.RedScriptEvaluator.instruction)
                  throw UNDEFINED
              }
            case c: Character   => throw UNDEFINED
            case other          => throw UNDEFINED
          }
        }
      }

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

        case Instruction(Key.PageUp,   _, NoModifier) => UI_DO(commands.selectMatchingUp)
        case Instruction(Key.PageDown, _, NoModifier) => UI_DO(commands.selectMatchingDown)

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
        case MouseDragged(row, col,    ControlButton1)  => UI_DO(commands.setCursor(row, col))
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
      val singleLineKeyboard: UserInputHandler = ignoreMultiLineKeys orElse keyboard

}
