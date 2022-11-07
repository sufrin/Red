package Red

/**
  * A bag of tools to support the simple and intelligible decoding of
  * `MouseButtonEvent`s and `KeyEvent`s generated by the underlying platform, as
  * well as the `UserInputDetail`s into which they are translated by
  * `InputPanel`s and their descendants.
  *
  *  The "little language" of modifier detail implemented here reduces
  *  (to zero) the number of imports needed elsewhere from modules defined in
  *  javax.swing. A typical use, during the decoding of an event with a `mods: Detail` field
  *  might be
  *
  * {{{
  *    mods.hasButton1 || mods.hasMeta  || mods.hasControl
  * }}}
  * or its equivalent
  * {{{
  *   mods.hasAny(Buttons.Button1|Buttons.Meta|Buttons.Control)
  * }}}
  *
  *
  */
object UserInputDetail {

  /** Intelligibly-decodable modifier details  */
  class Detail(val mods: Int) extends AnyVal {
    import Button._
    import Modifier._
    def hasAlt: Boolean     = (mods & Alt) != 0
    def hasMeta: Boolean    = (mods & Meta) != 0
    def hasShift: Boolean   = (mods & Shift) != 0
    def hasControl: Boolean = (mods & Control) != 0
    def hasButton1: Boolean = (mods & Button1) != 0
    def hasButton2: Boolean = (mods & Button2) != 0
    def hasButton3: Boolean = (mods & Button3) != 0

    def hasAll(bitMask: Int): Boolean  = (mods & bitMask) == bitMask
    def hasAny(bitMask: Int): Boolean  = (mods & bitMask) != 0
    def hasNone(bitMask: Int): Boolean = (mods & bitMask) == 0

    /** Replace Meta with Control */
    def mapMeta: Detail = if ((mods&Meta) == 0) this else new Detail((mods ^ Meta) | Control)

    def asText: String = {
      import Button._
      import Modifier._
      val b = new StringBuilder
      if (hasAny(Control)) b.append("C")
      if (hasAny(Shift)) b.append("S")
      if (hasAny(Alt)) b.append("A")
      if (hasAny(Meta)) b.append("M")
      if (hasAny(Button1)) b.append("B1")
      if (hasAny(Button2)) b.append("B2")
      if (hasAny(Button3)) b.append("B3")
      if (b.nonEmpty) b.insert(0, "|")
      b.toString
    }
    override def toString: String = s"Detail($asText)"
  }

  object Detail {
    def apply(modifiers: Int) = new Detail(modifiers)

    def withDetail(string: String): Option[Detail] = {
      import Modifiers._
      var mod: Int = 0
      def orWith(bit: Detail): Unit = { mod = mod | bit.mods }
      if (string.contains("C")) orWith(Control)
      if (string.contains("S")) orWith(Shift)
      if (string.contains("A")) orWith(Alt)
      if (string.contains("M")) orWith(Meta)
      if (string.contains("B1")) orWith(Button1)
      if (string.contains("B2")) orWith(Button2)
      if (string.contains("B3")) orWith(Button3)
      if (string.forall{ c => "CSAMB123".contains(c) }) Some(new Detail(mod)) else None
    }
  }
  /**
   *    Named button masks for use in the construction
   *    of Detail constants.
   */
  private object Button {
    import java.awt.event.InputEvent._
    val Button1: Int = BUTTON1_DOWN_MASK
    val Button2: Int = BUTTON2_DOWN_MASK
    val Button3: Int = BUTTON3_DOWN_MASK
  }


  /**
    * Alias for `swing.event.key`: to be used as a prefix `Key.A, Key.Numpad0`,
    * etc.
    */
  val Key: swing.event.Key.type = swing.event.Key

  /**
    *  Alias for `Key.Modifier`: to be used as a prefix, in building
    *  Detail constants eg. `Modifier.Shift`
    */
  private val Modifier: Key.Modifier.type = Key.Modifier

  /**
    *  Named Detail constants for use in direct pattern matching
    */
  object Modifiers {
    val Control: Detail          = Detail(Modifier.Control)
    val Shift: Detail            = Detail(Modifier.Shift)
    val Alt: Detail              = Detail(Modifier.Alt)
    val AltGraph: Detail         = Detail(Modifier.AltGraph)
    val Meta: Detail             = Detail(Modifier.Meta)
    val NoModifier: Detail       = Detail(0)
    val ControlShift: Detail     = Detail(Modifier.Control|Modifier.Shift)
    val AltShift: Detail         = Detail(Modifier.Alt|Modifier.Shift)
    val ControlMeta: Detail      = Detail(Modifier.Control|Modifier.Meta)
    val Button1: Detail          = Detail(Button.Button1)
    val Button2: Detail          = Detail(Button.Button2)
    val Button3: Detail          = Detail(Button.Button3)
    val ControlButton1: Detail   = Detail(Modifier.Control|Button.Button1)
    val ControlButton2: Detail   = Detail(Modifier.Control|Button.Button2)
    val ControlButton3: Detail   = Detail(Modifier.Control|Button.Button3)
  }

  /** Enumeration of the various keyboard locations */
  val Location: Key.Location.type = Key.Location
}
