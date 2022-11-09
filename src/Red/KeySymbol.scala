package Red

import scala.swing.event.Key

/**
 * Mappings from key names to their numeric identities and back
 * Circumvents a well-hidden bug in the reflective implementation of `Key.withName`
 */
object KeySymbol {

// for { k <- scala.swing.event.Key.values } println(f"\"$k%s\" ->  0x${k.id}%x,")

val pairs = Array(
            "Cancel" ->  0x3,
            "Backspace" ->  0x8,
            "Tab" ->  0x9,
            "Enter" ->  0xa,
            "Clear" ->  0xc,
            "Shift" ->  0x10,
            "Ctrl" ->  0x11,
            "Alt" ->  0x12,
            "Pause" ->  0x13,
            "Caps Lock" ->  0x14,
            "Kana" ->  0x15,
            "Final" ->  0x18,
            "Kanji" ->  0x19,
            "Escape" ->  0x1b,
            "Convert" ->  0x1c,
            "No Convert" ->  0x1d,
            "Accept" ->  0x1e,
            "Mode Change" ->  0x1f,
            "Space" ->  0x20,
            "Page Up" ->  0x21,
            "Page Down" ->  0x22,
            "End" ->  0x23,
            "Home" ->  0x24,
            "Left" ->  0x25,
            "Up" ->  0x26,
            "Right" ->  0x27,
            "Down" ->  0x28,
            "Comma" ->  0x2c,
            "Minus" ->  0x2d,
            "Period" ->  0x2e,
            "Slash" ->  0x2f,
            "0" ->  0x30,
            "1" ->  0x31,
            "2" ->  0x32,
            "3" ->  0x33,
            "4" ->  0x34,
            "5" ->  0x35,
            "6" ->  0x36,
            "7" ->  0x37,
            "8" ->  0x38,
            "9" ->  0x39,
            "Semicolon" ->  0x3b,
            "Equals" ->  0x3d,
            "A" ->  0x41,
            "B" ->  0x42,
            "C" ->  0x43,
            "D" ->  0x44,
            "E" ->  0x45,
            "F" ->  0x46,
            "G" ->  0x47,
            "H" ->  0x48,
            "I" ->  0x49,
            "J" ->  0x4a,
            "K" ->  0x4b,
            "L" ->  0x4c,
            "M" ->  0x4d,
            "N" ->  0x4e,
            "O" ->  0x4f,
            "P" ->  0x50,
            "Q" ->  0x51,
            "R" ->  0x52,
            "S" ->  0x53,
            "T" ->  0x54,
            "U" ->  0x55,
            "V" ->  0x56,
            "W" ->  0x57,
            "X" ->  0x58,
            "Y" ->  0x59,
            "Z" ->  0x5a,
            "Open Bracket" ->  0x5b,
            "Back Slash" ->  0x5c,
            "Close Bracket" ->  0x5d,
            "NumPad-0" ->  0x60,
            "NumPad-1" ->  0x61,
            "NumPad-2" ->  0x62,
            "NumPad-3" ->  0x63,
            "NumPad-4" ->  0x64,
            "NumPad-5" ->  0x65,
            "NumPad-6" ->  0x66,
            "NumPad-7" ->  0x67,
            "NumPad-8" ->  0x68,
            "NumPad-9" ->  0x69,
            "NumPad *" ->  0x6a,
            "NumPad +" ->  0x6b,
            "NumPad ," ->  0x6c,
            "NumPad -" ->  0x6d,
            "NumPad ." ->  0x6e,
            "NumPad /" ->  0x6f,
            "F1" ->  0x70,
            "F2" ->  0x71,
            "F3" ->  0x72,
            "F4" ->  0x73,
            "F5" ->  0x74,
            "F6" ->  0x75,
            "F7" ->  0x76,
            "F8" ->  0x77,
            "F9" ->  0x78,
            "F10" ->  0x79,
            "F11" ->  0x7a,
            "F12" ->  0x7b,
            "Delete" ->  0x7f,
            "Dead Grave" ->  0x80,
            "Dead Acute" ->  0x81,
            "Dead Circumflex" ->  0x82,
            "Dead Tilde" ->  0x83,
            "Dead Macron" ->  0x84,
            "Dead Breve" ->  0x85,
            "Dead Above Dot" ->  0x86,
            "Dead Diaeresis" ->  0x87,
            "Dead Above Ring" ->  0x88,
            "Dead Double Acute" ->  0x89,
            "Dead Caron" ->  0x8a,
            "Dead Cedilla" ->  0x8b,
            "Dead Ogonek" ->  0x8c,
            "Dead Iota" ->  0x8d,
            "Dead Voiced Sound" ->  0x8e,
            "Dead Semivoiced Sound" ->  0x8f,
            "Num Lock" ->  0x90,
            "Scroll Lock" ->  0x91,
            "Ampersand" ->  0x96,
            "Asterisk" ->  0x97,
            "Double Quote" ->  0x98,
            "Less" ->  0x99,
            "Print Screen" ->  0x9a,
            "Insert" ->  0x9b,
            "Help" ->  0x9c,
            "Meta" ->  0x9d,
            "Greater" ->  0xa0,
            "Left Brace" ->  0xa1,
            "Right Brace" ->  0xa2,
            "Back Quote" ->  0xc0,
            "Quote" ->  0xde,
            "Up" ->  0xe0,
            "Down" ->  0xe1,
            "Left" ->  0xe2,
            "Right" ->  0xe3,
            "Alphanumeric" ->  0xf0,
            "Katakana" ->  0xf1,
            "Hiragana" ->  0xf2,
            "Full-Width" ->  0xf3,
            "Half-Width" ->  0xf4,
            "Roman Characters" ->  0xf5,
            "All Candidates" ->  0x100,
            "Previous Candidate" ->  0x101,
            "Code Input" ->  0x102,
            "Japanese Katakana" ->  0x103,
            "Japanese Hiragana" ->  0x104,
            "Japanese Roman" ->  0x105,
            "Kana Lock" ->  0x106,
            "Input Method On/Off" ->  0x107,
            "At" ->  0x200,
            "Colon" ->  0x201,
            "Circumflex" ->  0x202,
            "Dollar" ->  0x203,
            "Euro" ->  0x204,
            "Exclamation Mark" ->  0x205,
            "Inverted Exclamation Mark" ->  0x206,
            "Left Parenthesis" ->  0x207,
            "Number Sign" ->  0x208,
            "Plus" ->  0x209,
            "Right Parenthesis" ->  0x20a,
            "Underscore" ->  0x20b,
            "Windows" ->  0x20c,
            "Context Menu" ->  0x20d,
            "F13" ->  0xf000,
            "F14" ->  0xf001,
            "F15" ->  0xf002,
            "F16" ->  0xf003,
            "F17" ->  0xf004,
            "F18" ->  0xf005,
            "F19" ->  0xf006,
            "F20" ->  0xf007,
            "F21" ->  0xf008,
            "F22" ->  0xf009,
            "F23" ->  0xf00a,
            "F24" ->  0xf00b,
            "Compose" ->  0xff20,
            "Begin" ->  0xff58,
            "Alt Graph" ->  0xff7e,
            "Stop" ->  0xffc8,
            "Again" ->  0xffc9,
            "Props" ->  0xffca,
            "Undo" ->  0xffcb,
            "Copy" ->  0xffcd,
            "Paste" ->  0xffcf,
            "Find" ->  0xffd0,
            "Cut" ->  0xffd1,
            )

        def keyIdWithName(string: String): Option[Int] =
          { var res: Option[Int] = None
            for { (name, value) <- pairs if res.isEmpty }
              if (name==string) res=Some(value)
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