package Red

import scala.collection.mutable

/**
 * Supplementary mapping of keycodes to characters: applied
 * only when  `Alt` or `Alt+Shift`  are in effect. This may be
 * changed ad-lib; it is initialized to a mapping from the roman
 * alphabet to corresponding greek symbols. Some Apple keymaps appropriate
 * the alt shift for their own purposes; so the alt-interpretation is (by default) turned off
 * in these.
 *
 * ===Warning:
 *
 * What follows is only for the very intrepid or the very desperate. Most
 * application programmers using a `InputPanel` will not need to stray into
 * any of the detailed explanation below.
 *
 * ===Detail
 *
 * Most operating systems provide for customization of the mapping
 * between physical keys and logical characters. Many of the
 * methods involve keystrokes made with an `Alt`-shift, and here
 * we attempt to address these.
 *
 * Characters typed with only  `Alt` or  `Alt+Shift` in effect are
 * treated specially:
 *
 * 1. It is assumed (as a first approximation) that the underlying
 * operating system has handled decoding of keys typed with `Alt` down,
 * and that the resulting character codes are intended to denote
 * ordinary Unicode `Character`s.
 *
 * 2. This assumption is sound for OS/X, but is currently known
 * not to be satisfied by many popular Linux variants. Moreover
 * even in OS/X it can be a lot of trouble to find, or
 * to use system facilities to craft, a mapping from the
 * physical keys on a keyboard to (Unicode) characters
 * -- if they happen to be outside a conventional alphabet.
 *
 * The `altKeyChar` mapping is used to override the parts of
 * the physical key to unicode mapping for which it is defined.
 * A character that was typed with the alt modifier down is returned
 * as a `Character` translated, as below, using this mapping
 *
 * {{{
 * val char =
 *   altKeyChar.getOrElse(shifted(keyCode,mods.hasShift),keyChar)
 * Character(char, location, mods)
 * }}}
 *
 * A ''physical key'' is specified as either `shifted`''(c)''
 * or `unshifted`''(c)'', where ''c'' is the letter on the
 * keytop. Here's a little example of the construction and
 * installation of such a mapping.
 *
 * Note that shifted/unshifted arguments are
 * letters marked on keytops (upper case)
 * {{{
 * altKeyChar = new HashMap[Int, Char]().addAll(List(
 *  unshifted('A') -> '\u03b1', // alpha
 *  unshifted('B') -> '\u03b2', // beta
 *  unshifted('C') -> '\u03b3', // gamma
 *  shifted('C') -> '\u0393',   // GAMMA
 *  unshifted('D') -> '\u03b4', // delta
 *  shifted('D') -> '\u0394',   // DELTA
 *  ))
 * }}}
 *
 * ===Advice
 *
 * It is a good idea to use `altKeyChar` maps sparingly.
 * There are much better, though more prolix, ways of
 * typing "exotic" symbols and sequences conveniently; one of which
 * is provided in the "abbreviation" facility of the complete
 * editor.
 *
 */
object AltKeyboard {
  //
  //
  //      TL;DR
  //
  //

  private val mathTrans = List(
    unshifted('A') -> '\u03b1', // alpha
    unshifted('B') -> '\u03b2', // beta
    unshifted('C') -> '\u03b3', // gamma
    shifted('C') -> '\u0393',   // GAMMA
    unshifted('D') -> '\u03b4', // delta
    shifted('D') -> '\u0394',   // DELTA
    unshifted('E') -> '\u03b5',
    unshifted('F') -> '\u03b6',
    unshifted('G') -> '\u03b7',
    unshifted('H') -> '\u03b8',
    shifted('H') -> '\u0398',  // THETA
    unshifted('I') -> '\u03b9',
    unshifted('J') -> '\u03b9',
    unshifted('K') -> '\u03ba',
    unshifted('L') -> '\u03bb',
    shifted('L') -> '\u039b',  // LAMBDA
    unshifted('M') -> '\u03bc',
    unshifted('N') -> '\u03bd',
    unshifted('Z') -> '\u03be', // Zeta
    unshifted('O') -> '\u03bf',
    unshifted('P') -> '\u03c0',
    unshifted('Q') -> '\u03c2',
    unshifted('R') -> '\u03c1',  // Rho
    unshifted('S') -> '\u03C3',  // Sigma
    shifted('S')   -> '\u03A3',  // SIGMA
    unshifted('T') -> '\u03C4',  // Tau
    unshifted('U') -> '\u03C5',  // Nu
    unshifted('V') -> '\u03C6',  // Phi
    unshifted('W') -> '\u03C9',  // omega
    unshifted('X') -> '\u03C7',  // xi
    unshifted('Y') -> '\u03C8',  // Psi
    unshifted('Z') -> '\u03C9',
    shifted('<') -> '\u2264',   // <=
    shifted('>') -> '\u2265',   // >=
    unshifted('-') -> '\u00ac',   // logical not
    shifted('=') -> '\u2262',   // IDENTICAL TO
    unshifted('=') -> '\u2261'  // NOT IDENTICAL TO
  )

  var altKeyChar: mutable.Map[Int, Char] = new mutable.HashMap[Int, Char]().addAll(mathTrans)

  def mapTo(from: Char, to: Char, shift: Boolean): Unit =
    altKeyChar.addOne((if (shift) shifted(from) else unshifted(from), to))

  def clear(): Unit = altKeyChar.clear()

  /** An arbitrary bitpattern beyond Char.MaxValue */
  private val SHIFTED: Int = Char.MaxValue+1
  /** Local integer encoding of a character; used
   *  in constructing and decoding `altKeyChar` maps:
   *
   *  Yields an integer derived from `char`, distinct from
   *  any `Char`.toInt value; unless `shifted` is false, in
   *  which case it yields `char`
   */
  @inline def shifted(char: Int, shifted: Boolean=true): Int =
    if (shifted) SHIFTED|char else char

  /** Yields `char` (identical to `shifted(char, false)` */
  @inline def unshifted(char: Int): Int = char

  /**
   *  If an one of these diacritical characters appears with an alt set then
   *  we expect that an underlying (Mac) keyboard will deliver
   *  a precomposed ligature character on the next keystroke.
   */
  var macKeyboardDiacritical: String = "´`^¨"
}
