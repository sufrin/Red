package Red

import scala.swing.event.Key

/**
 * Mappings (implemented by linear search) from key names to their numeric identities and back.
 * Circumvents a well-hidden bug in the reflective implementation of `Key.withName`
 *
 * Official names have spaces and capitals in them; I have added unofficial variants as
 * reserves with neither spaces nor capitals. Names are normalized to lower case.
 */
object KeySymbol {

// for { k <- scala.swing.event.Key.values } println(f"\"$k%s\" ->  0x${k.id}%x,")

val pairs: List[(String, Int)] = List(
            "CANCEL" ->  0X3,
            "BACKSPACE" ->  0X8,
            "TAB" ->  0X9,
            "ENTER" ->  0XA,
            "CLEAR" ->  0XC,
            "SHIFT" ->  0X10,
            "CTRL" ->  0X11,
            "ALT" ->  0X12,
            "PAUSE" ->  0X13,
            "CAPS LOCK" ->  0X14,
            "KANA" ->  0X15,
            "FINAL" ->  0X18,
            "KANJI" ->  0X19,
            "ESCAPE" ->  0X1B,
            "CONVERT" ->  0X1C,
            "NO CONVERT" ->  0X1D,
            "ACCEPT" ->  0X1E,
            "MODE CHANGE" ->  0X1F,
            "SPACE" ->  0X20,
            "PAGE UP" ->  0X21,
            "PAGE DOWN" ->  0X22,
            "END" ->  0X23,
            "HOME" ->  0X24,
            "LEFT" ->  0X25,
            "UP" ->  0X26,
            "RIGHT" ->  0X27,
            "DOWN" ->  0X28,
            "COMMA" ->  0X2C,
            "MINUS" ->  0X2D,
            "PERIOD" ->  0X2E,
            "SLASH" ->  0X2F,
            "0" ->  0X30,
            "1" ->  0X31,
            "2" ->  0X32,
            "3" ->  0X33,
            "4" ->  0X34,
            "5" ->  0X35,
            "6" ->  0X36,
            "7" ->  0X37,
            "8" ->  0X38,
            "9" ->  0X39,
            "SEMICOLON" ->  0X3B,
            "EQUALS" ->  0X3D,
            "A" ->  0X41,
            "B" ->  0X42,
            "C" ->  0X43,
            "D" ->  0X44,
            "E" ->  0X45,
            "F" ->  0X46,
            "G" ->  0X47,
            "H" ->  0X48,
            "I" ->  0X49,
            "J" ->  0X4A,
            "K" ->  0X4B,
            "L" ->  0X4C,
            "M" ->  0X4D,
            "N" ->  0X4E,
            "O" ->  0X4F,
            "P" ->  0X50,
            "Q" ->  0X51,
            "R" ->  0X52,
            "S" ->  0X53,
            "T" ->  0X54,
            "U" ->  0X55,
            "V" ->  0X56,
            "W" ->  0X57,
            "X" ->  0X58,
            "Y" ->  0X59,
            "Z" ->  0X5A,
            "OPEN BRACKET" ->  0X5B,
            "BACK SLASH" ->  0X5C,
            "CLOSE BRACKET" ->  0X5D,
            "NUMPAD-0" ->  0X60,
            "NUMPAD-1" ->  0X61,
            "NUMPAD-2" ->  0X62,
            "NUMPAD-3" ->  0X63,
            "NUMPAD-4" ->  0X64,
            "NUMPAD-5" ->  0X65,
            "NUMPAD-6" ->  0X66,
            "NUMPAD-7" ->  0X67,
            "NUMPAD-8" ->  0X68,
            "NUMPAD-9" ->  0X69,
            "NUMPAD *" ->  0X6A,
            "NUMPAD +" ->  0X6B,
            "NUMPAD ," ->  0X6C,
            "NUMPAD -" ->  0X6D,
            "NUMPAD ." ->  0X6E,
            "NUMPAD /" ->  0X6F,
            "F1" ->  0X70,
            "F2" ->  0X71,
            "F3" ->  0X72,
            "F4" ->  0X73,
            "F5" ->  0X74,
            "F6" ->  0X75,
            "F7" ->  0X76,
            "F8" ->  0X77,
            "F9" ->  0X78,
            "F10" ->  0X79,
            "F11" ->  0X7A,
            "F12" ->  0X7B,
            "DELETE" ->  0X7F,
            "DEAD GRAVE" ->  0X80,
            "DEAD ACUTE" ->  0X81,
            "DEAD CIRCUMFLEX" ->  0X82,
            "DEAD TILDE" ->  0X83,
            "DEAD MACRON" ->  0X84,
            "DEAD BREVE" ->  0X85,
            "DEAD ABOVE DOT" ->  0X86,
            "DEAD DIAERESIS" ->  0X87,
            "DEAD ABOVE RING" ->  0X88,
            "DEAD DOUBLE ACUTE" ->  0X89,
            "DEAD CARON" ->  0X8A,
            "DEAD CEDILLA" ->  0X8B,
            "DEAD OGONEK" ->  0X8C,
            "DEAD IOTA" ->  0X8D,
            "DEAD VOICED SOUND" ->  0X8E,
            "DEAD SEMIVOICED SOUND" ->  0X8F,
            "NUM LOCK" ->  0X90,
            "SCROLL LOCK" ->  0X91,
            "AMPERSAND" ->  0X96,
            "ASTERISK" ->  0X97,
            "DOUBLE QUOTE" ->  0X98,
            "LESS" ->  0X99,
            "PRINT SCREEN" ->  0X9A,
            "INSERT" ->  0X9B,
            "HELP" ->  0X9C,
            "META" ->  0X9D,
            "GREATER" ->  0XA0,
            "LEFT BRACE" ->  0XA1,
            "RIGHT BRACE" ->  0XA2,
            "BACK QUOTE" ->  0XC0,
            "QUOTE" ->  0XDE,
            "UP" ->  0XE0,
            "DOWN" ->  0XE1,
            "LEFT" ->  0XE2,
            "RIGHT" ->  0XE3,
            "ALPHANUMERIC" ->  0XF0,
            "KATAKANA" ->  0XF1,
            "HIRAGANA" ->  0XF2,
            "FULL-WIDTH" ->  0XF3,
            "HALF-WIDTH" ->  0XF4,
            "ROMAN CHARACTERS" ->  0XF5,
            "ALL CANDIDATES" ->  0X100,
            "PREVIOUS CANDIDATE" ->  0X101,
            "CODE INPUT" ->  0X102,
            "JAPANESE KATAKANA" ->  0X103,
            "JAPANESE HIRAGANA" ->  0X104,
            "JAPANESE ROMAN" ->  0X105,
            "KANA LOCK" ->  0X106,
            "INPUT METHOD ON/OFF" ->  0X107,
            "AT" ->  0X200,
            "COLON" ->  0X201,
            "CIRCUMFLEX" ->  0X202,
            "DOLLAR" ->  0X203,
            "EURO" ->  0X204,
            "EXCLAMATION MARK" ->  0X205,
            "INVERTED EXCLAMATION MARK" ->  0X206,
            "LEFT PARENTHESIS" ->  0X207,
            "NUMBER SIGN" ->  0X208,
            "PLUS" ->  0X209,
            "RIGHT PARENTHESIS" ->  0X20A,
            "UNDERSCORE" ->  0X20B,
            "WINDOWS" ->  0X20C,
            "CONTEXT MENU" ->  0X20D,
            "F13" ->  0XF000,
            "F14" ->  0XF001,
            "F15" ->  0XF002,
            "F16" ->  0XF003,
            "F17" ->  0XF004,
            "F18" ->  0XF005,
            "F19" ->  0XF006,
            "F20" ->  0XF007,
            "F21" ->  0XF008,
            "F22" ->  0XF009,
            "F23" ->  0XF00A,
            "F24" ->  0XF00B,
            "COMPOSE" ->  0XFF20,
            "BEGIN" ->  0XFF58,
            "ALT GRAPH" ->  0XFF7E,
            "STOP" ->  0XFFC8,
            "AGAIN" ->  0XFFC9,
            "PROPS" ->  0XFFCA,
            "UNDO" ->  0XFFCB,
            "COPY" ->  0XFFCD,
            "PASTE" ->  0XFFCF,
            "FIND" ->  0XFFD0,
            "CUT" ->  0XFFD1,
            )

            val normalised: Seq[(String, Int)] =
              pairs.filter{ case (name, value) => name.contains(' ')}.map{ case (name, value) => (name.replace(" ", ""),value)}

        def keyIdWithName(string: String): Option[Int] =
          { var res: Option[Int] = None
            val sought = string.toUpperCase
            for { (name, value) <- pairs if res.isEmpty }
              if (name==sought) res=Some(value)

            if (res.isEmpty)
               for { (name, value) <- normalised if res.isEmpty }
                    if (name==sought) res=Some(value)
  
            res
          }

        def keyWithName(string: String): Option[Key.Value] =
          keyIdWithName(string) match {
            case Some(id) => Some(Key(id))
            case None     => None
          }

        def nameForKeyId(int: Int): String =
        { var res = ""
          for { (name, value) <- pairs if res==""}
            if (value==int) res=name
          if (res==0) f"\\x$int%x" else res
        }

        def nameForKey(key: Key.Value): String =
            nameForKeyId(key.id)
}