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
      f"Character('$character%s', $location%s, ${mods.asText}%s) ($toInput)"

    override def toInput: String =
      f"'$character%s'${mods.asText}%s@$location%s"

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
      f"${KeySymbol.nameForKey(key)}%s${mods.asText}%s@$location%s"

    override def toString: String =
      f"Instruction($key, $location%s, ${mods.asText}%s) ($toInput)"
  }

  case class Diacritical(mark: Char )  extends UserInput {
    override def toString: String =
      s"Diacritical: $mark"
  }

  case class Undefined(fromText: String) extends UserInput

/**
 * `UserInput(description)` yields an `Instruction` or a `Character` suitable
 * for use in a mapping from input events to actions.
 *
 * Description Notation:
 *
 *   ''key''`(`''mods''`)@``''location'' -- for `Character`` keystrokes
 *
 *   `'`''ch''`'(`''mods''`)@``''location'' -- for `Instruction` keystrokes
 *
 *   A ''ch'' can be specified as a literal or as its unicode `\uhhhh` code.
 *
 *   A ''key'' can be specified by its name or as its hexadecimal `\xhhhh` identity code.
 *
 *   The ''key'' names (and corresponding identity codes) are given at the foot of this file.
 *   For example `F11`, `Left Parenthesis`, `A`.
 *
 *   The ''location'' must be named: names are `Standard`, `Numpad`, `Unknown`, `Left`, `Right` (etc).
 *
 *   Either or both the  `(`''mods''`)` and `@`''location'' fields may be elided. A missing
 *   ''location'' is taken to be `Standard`.
 *
 */
object UserInput {
  var theKey: Key.Value                      = _
  var theLoc: UserInputDetail.Location.Value = _
  var theDet: UserInputDetail.Detail         = _
  var theChar: Char                          = _
  val error = new collection.mutable.StringBuilder
  val NoModifier                             = UserInputDetail.Modifiers.NoModifier

  def isKey(keyName: String): Boolean = {
      //theKey = Key.withName(keyName)
      KeySymbol.keyWithName(keyName) match {
        case Some(key) =>
          theKey = key
          true
        case None =>
          if (keyName.startsWith("\\x") && isHexKey(keyName.drop(2))) true
          else {
            error.append(s"$keyName is neither a key name or a valid hexadecimal key specification")
            false
          }
      }
  }

  def isDet(detail: String): Boolean =
    Detail.withDetail(detail) match {
      case None => error.append(s"($detail) isn't modifier detail");  false
      case Some(detail) => theDet = detail; true
    }

  def isLoc(locName: String): Boolean = try {
      theLoc = Location.withName(locName)
      true
  } catch {
    case _ =>
      theLoc = Location.Standard
      if (locName!="") error.append(s"$locName isn't a keyboard location")
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
      case None => error.append(s"~isUnicode($string)"); false
      case Some(char) => theChar = char; true
    }
  }

  def isChar(string: String): Boolean = {
    val res =
    if (string.length==1)                    { theChar = string(0); true }
    else
    if (string.length==2 && string(0)=='\\') { theChar=string(1); true }
    else
      isUnicode(string)

    res
  }

  /**
   *  An `InputPanel` to be used with these key descriptors
   *  must have this/these values
   */
  var numpadAsCommand, mapMeta: Boolean = true

  case class Fields(isInstruction: Boolean, var loc: Option[String], var decode: Option[String], var mods: Option[String])

  def fields(text: String):Fields = {
    text match {

      case s"'$key'($mods)@$loc" => Fields(false, Some(loc),     Some(key), Some(mods))
      case s"'$key'($mods)"      => Fields(false, None,          Some(key), Some(mods))
      case s"'$key'@$loc"        => Fields(false, Some(loc),     Some(key), None)
      case s"'$key'"             => Fields(false, None,          Some(key), None)
      case s"$key($mods)@$loc"   => Fields(true, Some(loc),      Some(key.intern), Some(mods))
      case s"$key($mods)"        => Fields(true, None,           Some(key.intern), Some(mods))
      case s"$key@$loc"          => Fields(true, Some(loc),      Some(key.intern), None)
      case _ => Fields(false, None, None, None)
    }
  }

  def parse(text: String): UserInput = {
    error.clear()
    var f = fields(text)
    if (f.loc.isEmpty) f.loc = Some("Standard")
    if (f.mods.isEmpty) f.mods = Some("") // No Modifier
    if (f.decode.isEmpty) Undefined(s"\"$text\" [No key specified]") else {
      val (Fields(isInstruction, Some(loc), Some(decode), Some(mods))) = f
      if (isInstruction && isLoc(loc) && isKey(decode) && isDet(mods))
        Instruction(theKey, theLoc, theDet)
      else {
      if (!isInstruction && isLoc(loc) && isChar(decode) && isDet(mods))
        Character(theChar, theLoc, theDet)
      else
        Undefined(s"\"$text\" ${error.toString()}")
      }
    }
  }

  def apply(text: String): UserInput = parse(text) match {
    case Character(char, location, detail) if false && detail.hasControl || detail.hasMeta || (location == Key.Location.Numpad && numpadAsCommand) =>
         // Linux and OS/X are consistent about e.KeyCode from a numpad, but not about e.getExtendKeyCode
         Instruction(Key(char.toInt), location, if (mapMeta) detail.mapMeta else detail)
    case other => other
  }

}

/** Key names and their codes

0=\x30 1=\x31 2=\x32 3=\x33 4=\x34 5=\x35 6=\x36 7=\x37 8=\x38
9=\x39 A=\x41 Accept=\x1e Again=\xffc9 All Candidates=\x100
Alphanumeric=\xf0 Alt Graph=\xff7e Alt=\x12 Ampersand=\x96 Asterisk=\x97
At=\x200 B=\x42 Back Quote=\xc0 Back Slash=\x5c Backspace=\x8
Begin=\xff58 C=\x43 Cancel=\x3 Caps Lock=\x14 Circumflex=\x202
Clear=\xc Close Bracket=\x5d Code Input=\x102 Colon=\x201 Comma=\x2c
Compose=\xff20 Context Menu=\x20d Convert=\x1c Copy=\xffcd Ctrl=\x11
Cut=\xffd1 D=\x44 Dead Above Dot=\x86 Dead Above Ring=\x88 Dead
Acute=\x81 Dead Breve=\x85 Dead Caron=\x8a Dead Cedilla=\x8b Dead
Circumflex=\x82 Dead Diaeresis=\x87 Dead Double Acute=\x89 Dead
Grave=\x80 Dead Iota=\x8d Dead Macron=\x84 Dead Ogonek=\x8c Dead
Semivoiced Sound=\x8f Dead Tilde=\x83 Dead Voiced Sound=\x8e
Delete=\x7f Dollar=\x203 Double Quote=\x98 Down=\x28 Down=\xe1
E=\x45 End=\x23 Enter=\xa Equals=\x3d Escape=\x1b Euro=\x204
Exclamation Mark=\x205 F=\x46 F1=\x70 F10=\x79 F11=\x7a F12=\x7b
F13=\xf000 F14=\xf001 F15=\xf002 F16=\xf003 F17=\xf004 F18=\xf005
F19=\xf006 F2=\x71 F20=\xf007 F21=\xf008 F22=\xf009 F23=\xf00a
F24=\xf00b F3=\x72 F4=\x73 F5=\x74 F6=\x75 F7=\x76 F8=\x77 F9=\x78
Final=\x18 Find=\xffd0 Full-Width=\xf3 G=\x47 Greater=\xa0 H=\x48
Half-Width=\xf4 Help=\x9c Hiragana=\xf2 Home=\x24 I=\x49 Input
Method On/Off=\x107 Insert=\x9b Inverted Exclamation Mark=\x206
J=\x4a Japanese Hiragana=\x104 Japanese Katakana=\x103 Japanese
Roman=\x105 K=\x4b Kana Lock=\x106 Kana=\x15 Kanji=\x19 Katakana=\xf1
L=\x4c Left Brace=\xa1 Left Parenthesis=\x207 Left=\x25 Left=\xe2
Less=\x99 M=\x4d Meta=\x9d Minus=\x2d Mode Change=\x1f N=\x4e No
Convert=\x1d Num Lock=\x90 NumPad *=\x6a NumPad +=\x6b NumPad ,=\x6c
NumPad -=\x6d NumPad .=\x6e NumPad /=\x6f NumPad-0=\x60 NumPad-1=\x61
NumPad-2=\x62 NumPad-3=\x63 NumPad-4=\x64 NumPad-5=\x65 NumPad-6=\x66
NumPad-7=\x67 NumPad-8=\x68 NumPad-9=\x69 Number Sign=\x208 O=\x4f
Open Bracket=\x5b P=\x50 Page Down=\x22 Page Up=\x21 Paste=\xffcf
Pause=\x13 Period=\x2e Plus=\x209 Previous Candidate=\x101 Print
Screen=\x9a Props=\xffca Q=\x51 Quote=\xde R=\x52 Right Brace=\xa2
Right Parenthesis=\x20a Right=\x27 Right=\xe3 Roman Characters=\xf5
S=\x53 Scroll Lock=\x91 Semicolon=\x3b Shift=\x10 Slash=\x2f
Space=\x20 Stop=\xffc8 T=\x54 Tab=\x9 U=\x55 Underscore=\x20b
Undo=\xffcb Up=\x26 Up=\xe0 V=\x56 W=\x57 Windows=\x20c X=\x58
Y=\x59 Z=\x5a
*/