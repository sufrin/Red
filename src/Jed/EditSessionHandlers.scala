package Jed

import Red.UserInputDetail.Key
import Red.UserInputDetail.Modifiers._
import Red._

/**
 *  This class defines handlers for keyboard and mouse events.
 *  The composite `keyboard` and `mouse` handlers defined here
 *  invoke appropriate `Command`s defined by `EditSessionCommands` with `DO`.
 *
 *  The handler `singleLineKeyboard` responds the same as `keyboard` except
 *  for `Key.Up`, `Key.Down`, and `'\n'`, to which it responds by doing nothing.
 *
 *  `DO` should execute its argument `command`, by invoking `command.DO` on
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
class EditSessionHandlers(val DO: Commands.Command[EditSession]=>Unit) {
  
  private val commands = EditSessionCommands
  
  type UserInputHandler = Notifier.Handler[UserInput]
      val indentOrTab = commands.autoIndentSelection ||| commands.autoTab

      val keyboard: UserInputHandler =  {
        case Character(char, _, NoModifier)            => DO(commands.insertCommand(char))
        case Character(char, _, Shift)                 => DO(commands.insertCommand(char))
        case Character(char, _, mods) if mods.hasAlt   => DO(commands.insertCommand(char))

        case Instruction(Key.Q, _, Control)            =>

        case Instruction(Key.Tab, _, NoModifier)       => DO(indentOrTab)
        case Instruction(Key.Tab, _, Shift)            => DO(commands.undentSelection)

        case Instruction(Key.BackSpace, _, NoModifier) => DO(commands.delete)
        case Instruction(Key.BackSpace, _, Control)    => DO(commands.flip)

        case Instruction(Key.X, _, Control)       => DO(commands.cut)
        case Instruction(Key.C, _, Control)       => DO(commands.copy)
        case Instruction(Key.V, _, Control)       => DO(commands.paste)
        case Instruction(Key.B, _, Control)       => DO(commands.exchangeCut)

        // Sufrin's favourites
        case Instruction(Key.F1, _, NoModifier)       => DO(commands.cut)
        case Instruction(Key.F3, _, NoModifier)       => DO(commands.copy)
        case Instruction(Key.F2, _, NoModifier)       => DO(commands.paste)
        case Instruction(Key.F4, _, NoModifier)       => DO(commands.exchangeCut)
        case Instruction(Key.F12, _, NoModifier)      => DO(commands.exchangeMark)

        case Instruction(Key.Home, _, NoModifier) => DO(commands.toHome)
        case Instruction(Key.End, _, NoModifier)  => DO(commands.toEnd)
        case Instruction(Key.A, _, Control)       => DO(commands.selectAll)

        case Instruction(Key.Left, _, NoModifier)  => DO(commands.prevChar)
        case Instruction(Key.Right, _, NoModifier) => DO(commands.nextChar)
        case Instruction(Key.Down, _, NoModifier)  => DO(commands.nextLine)
        case Instruction(Key.Up, _, NoModifier)    => DO(commands.prevLine)

        case Instruction(Key.PageUp,   _, NoModifier) => DO(commands.selectMatchingUp)
        case Instruction(Key.PageDown, _, NoModifier) => DO(commands.selectMatchingDown)
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
        case MousePressed(row, col, 1, Button1)           => DO(commands.setCursorAndMark(row, col))
        case MousePressed(row, col, n, Button1)  if 2<=n  => DO(commands.selectChunk(row, col, n))
        case MouseDragged(row, col,    Button1)           => DO(commands.dragCursor(row, col))
        case MouseReleased(_, _,       Button1)           => DO(commands.mouseUp)

        case MousePressed(row, col, n, ControlButton1)  => DO(commands.setCursor(row, col))
        case MouseDragged(row, col,    ControlButton1)  => DO(commands.setCursor(row, col))
        case MouseReleased(_, _,       ControlButton1)  => DO(commands.mouseUp)

        case MousePressed(row, col, n, detail) if detail.hasButton3 => DO(commands.setMark(row, col))

        // Multiple presses
        case MousePressed(_, _, _, Button1)  => ()
        case MousePressed(_, _, _, Button3)  => ()

        case MouseReleased(_, _, _)          => ()
        case MouseDragged(_, _, _)           => ()
      }

      /**
       *   A handler that does nothing in response to `Key.Up`, `Key.Down`, and `'\n'`;
       *   and treats tab as space
       */
      private val ignoreMultiLineKeys: UserInputHandler = {
        case Character('\n', _, _)       => ()
        case Instruction(Key.Down, _, _) => ()
        case Instruction(Key.Up, _, _)   => ()
        case Instruction(Key.Tab, _, _)  => DO(commands.insertCommand(' '))
      }

      /**
       *  A handler that does nothing in response to `Key.Up`, `Key.Down`, and `'\n'` but otherwise responds to
       *  keys in the same way as  `keyboard`
       */
      val singleLineKeyboard: UserInputHandler = ignoreMultiLineKeys orElse keyboard

}
