package Red

import UserInputDetail._

import scala.swing.event.Key

/**  Events generated from `DocumentView` and other components as a result
  *  of user interaction with keyboard and mouse.
  */

  sealed abstract class UserInput
  /**
   * A mouse key was pressed `count` times  at `(row,col)` of the
   * model being viewed by the component.
   */
  case class MousePressed(row: Int, col: Int, count: Int, mods: Detail)
    extends UserInput

  /**
   *  A mouse key was released in  at `(row,col)` of the
   *  model being viewed by the component.
   */
  case class MouseReleased(row: Int, col: Int, mods: Detail)
    extends UserInput

  /**
    * The mouse was dragged (ie moved with one of its buttons down)
    * at `(row,col)` of the model being viewed by the component.
    */
  case class MouseDragged(row: Int, col: Int, mods: Detail)
    extends UserInput

  /** The mouse wheel was turned by `rotation` clicks.
   */
  case class MouseWheel(rotation: Int, Detail: Int)
    extends UserInput

  case object MouseEntered extends UserInput

  case object MouseExited  extends UserInput


  /** An "ordinary" character, `char` was typed in the component.
   *  The `location` can usually be ignored, unless it
   *  is {{{Key.Location.NumPad}}}
   */
  case class Character
       (  char:     Char,
          location: Key.Location.Value,
          mods:     Detail
       )
    extends UserInput {
    @inline def character: String =
      if (char.isControl)
        s"^${(char + 'A' - 1).toChar.toString}"
      else
        char.toString
    override def toString: String =
      s"Character: ${mods.asText}'$character'@ $location)"
  }

  /** A keystroke deemed to be an action was typed.
   *  The `location` can usually be safely ignored, unless it
   *  is {{{Key.Location.NumPad}}}
   */
  case class Instruction
     (  key:      Key.Value,
        location: Key.Location.Value,
        mods:     Detail)
    extends UserInput {
    override def toString: String =
      s"Instruction: ${mods.asText}Key.$key @ $location"
  }

