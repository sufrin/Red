package Red

import Red.UserInputDetail._

import scala.swing.event.Key

/**  Events generated from `DocumentView` and other components as a result
  *  of user interaction with keyboard and mouse.
  */

  sealed abstract class UserInput {
      def toInput: String = toString
  }
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
        f"\\u${char.toInt}%04x"
      else
        char.toString
    override def toString: String =
      f"Character($location%s.'$character%s'${mods.asText}%s) ($toInput)"

    override def toInput: String =
      f"\"$location%s.\\u${char.toInt}%04x${mods.asText}%s\""

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
    override def toInput: String =
      f"\"$location%s.\\x${key.id}%04x${mods.asText}%s\""
    override def toString: String =
      f"Instruction($location%s.$key${mods.asText}%s) ($toInput)"
  }

  case class Diacritical(mark: Char )  extends UserInput {
    override def toString: String =
      s"Diacritical: $mark"
  }

  case class Undefined(fromText: String) extends UserInput

object UserInput {
  var theKey: Key.Value                      = _
  var theLoc: UserInputDetail.Location.Value = _
  var theDet: UserInputDetail.Detail         = _
  var theChar: Char                          = _
  val NoModifier                             = UserInputDetail.Modifiers.NoModifier

  def isKey(keyName: String): Boolean = try {
      theKey = Key.withName(keyName)
      true
  } catch {
    case _ =>
      keyName.startsWith("\\x") && isHexKey(keyName.drop(2))
  }

  def isDet(detail: String): Boolean =
    Detail.withDetail(detail) match {
      case None => false
      case Some(detail) => theDet = detail; true
    }

  def isLoc(locName: String): Boolean = try {
      theLoc= Location.withName(locName)
      true
  } catch {
    case _ =>
      theLoc = Location.Standard
      locName==""
  }

  def isHexKey(string: String): Boolean = try {
    import Useful.CharSequenceOperations._
    string.hexToLong match {
      case None => false
      case Some(long) =>
        theKey = Key(long.toInt)
        true
    }
  } catch {
    case _ =>
      false
  }

  def isUnicode(string: String): Boolean = {
    import Useful.CharSequenceOperations._
    string.toUnicode match {
      case None => false
      case Some(char) => theChar = char; true
    }
  }

  def isChar(string: String): Boolean =
    if (string.length==1) { theChar = string(0); true }
    else
    if (string.length==2 && string(0)=='\\') { theChar=string(1); true }
    else
      isUnicode(string)

  /**
   * An `InputPanel` to be used with these key descriptors
   * must have this/these values
   */
  var numpadAsCommand, mapMeta: Boolean = true


  def parse(text: String): UserInput =
    text match {
      case s"$loc.$key" if isLoc(loc) && isKey(key) => Instruction(theKey, theLoc, NoModifier)
      case s"$loc.$key|$detail" if isLoc(loc) && isKey(key) && isDet(detail)=> Instruction(theKey, theLoc, theDet)

      case s"'$ch'" if isChar(ch) => Character(theChar, Location.Standard, NoModifier)
      case s"$loc.'$ch'|$detail" if isChar(ch) && isLoc(loc) && isDet(detail) => Character(theChar, theLoc, theDet)
      case s"'$ch'|$detail" if isChar(ch) && isDet(detail) => Character(theChar, Location.Standard, theDet)
      case _ => Undefined(text)
    }

  def apply(text: String): UserInput = parse(text) match {
    case Character(char, location, detail) if detail.hasControl || detail.hasMeta || (location == Key.Location.Numpad && numpadAsCommand) =>
         // Linux and OS/X are consistent about e.KeyCode from a numpad, but not about e.getExtendKeyCode
         Instruction(Key(char.toInt), location, if (mapMeta) detail.mapMeta else detail)
    case other => other
  }

}
