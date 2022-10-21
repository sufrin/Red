package Red

import Red.Utils.appleRedUI

import java.awt.Font
import scala.swing.{
  Action,
  Alignment,
  BoxPanel,
  Button,
  ButtonGroup,
  CheckMenuItem,
  Component,
  Label,
  MenuItem,
  Orientation,
  event
}

object Buttons {

  def Item(
      name: String,
      toolTip: String = "",
      theFont: Font = Utils.menuButtonFont
  )(act: => Unit): MenuItem =
    new MenuItem(Action(name) {
      act
    }) {
      font = theFont
      if (toolTip.nonEmpty) tooltip = toolTip
    }

  def Button(
      name: String,
      toolTip: String = "",
      theFont: Font = Utils.menuButtonFont
  )(act: => Unit): swing.Button =
    new Button(Action(name) {
      act
    }) {
      font = theFont
      if (toolTip.nonEmpty) tooltip = toolTip
      horizontalAlignment = Alignment.Center
      verticalAlignment = Alignment.Center
    }

  /** @return a centred label
    *  TODO: Make this more efficient. It is only
    *        a composite because layout parameters in swing are
    *        confusing
    */
  class CentredLabel(var _text: String, theFont: Font = Utils.rootFont)
      extends BoxPanel(Orientation.Horizontal) {
    val component = new Label(_text) { font = theFont }
    contents += Red.Glue.horizontal()
    contents += component
    contents += Red.Glue.horizontal()
    override def font_=(font: Font): Unit = { component.font = font }
    def text_=(_text: String): Unit = component.text = _text
    def setText(_text: String): Unit = component.text = _text
  }

  /** A centred button */
  class CentredButton(
      title: String,
      tip: String = "",
      theFont: Font = Utils.rootFont,
      act: => Unit
  ) extends BoxPanel(Orientation.Horizontal) {
    val component = Button(title) { act }
    component.tooltip = tip
    component.font = theFont
    contents += Red.Glue.horizontal()
    contents += component
    contents += Red.Glue.horizontal()
    override def font_=(font: Font): Unit = { component.font = font }
    def text_=(_text: String): Unit = component.text = _text
    def setText(_text: String): Unit = component.text = _text
  }

  /**
   * A checkbox whose current state is stored in the
   * persistent store.
   *
   * @param title title on the checkbox
   * @param persistentName name in the persistent store
   * @param set invoke whenever the checkitem is clicked
   * @param default initial value
   */
  class PersistentCheckItem(
      title: String,
      persistentName: String,
      set: Boolean => Unit,
      default: => Boolean
  ) extends CheckMenuItem(title) {
    selected = appleRedUI.getBoolean(persistentName, default)
    reactions += { case event.ButtonClicked(_) =>
      set(selected)
      appleRedUI.putBoolean(persistentName, selected)
      appleRedUI.sync()
    }
  }

  /**   A group of mutually-exclusive (menu-)items: each has a name and a value
    *   and (possibly) a tooltip. The group has an associated value, which
    *   is set whenever one of the group items is set.
    *
    *   @param _value the initial value associated with the group. The initially
    *                 selected item of the group is one of those, if any,
    *                 with this value.
    */
  abstract class Group(_value: String = "") extends ButtonGroup() { group =>

    /** The current value of the selected item */
    var value: String = _value

    /**  Invoked whenever an item from this group is selected.
      *  @param value will be the `value` of the selected menu item.
      */
    protected def select(value: String): Unit

    /**  Invoke the select method ''as if'' the currently-selected item had been clicked.
      */
    def select(): Unit = select(value)

    /**   Construct a menu item that is part of the group, and whose value is assigned to the
      *   group's value when it is clicked. The item '''must''' be added to a menu.
      *   It is customary, though not mandatory, for all the menu items in a group to be
      *   added to the same menu.
      */
    def CheckBox(
        name: String,
        value: String,
        toolTip: String = ""
    ): Component = {
      val it =
        new scala.swing.CheckBox(name) {
          selected = false
          font = Utils.menuButtonFont
          if (toolTip.nonEmpty) tooltip = toolTip
          listenTo(this)
          reactions += { case event.ButtonClicked(_) =>
            if (selected) {
              group.value = value
              select(value)
            }
          }
        }
      group.buttons += it
      if (value == group.value) group.select(it)
      it
    }
  }

  abstract class CheckBox(name: String, value: String, toolTip: String = "")
      extends scala.swing.CheckBox(name) {
    def click(): Unit
    selected = false
    font = Utils.menuButtonFont
    if (toolTip.nonEmpty) tooltip = toolTip
    listenTo(this)
    reactions += { case event.ButtonClicked(_) =>
      click()
    }
  }

  def menuButton(name: String, toolTip: String = "", centred: Boolean = false)(act: => Unit): MenuItem =
    new MenuItem(Action(name) { act }) {
      font = Utils.menuButtonFont
      if (centred) {
        xLayoutAlignment = 0.5
        horizontalAlignment = Alignment.Center
      }
      if (toolTip.nonEmpty) tooltip = toolTip
    }

  def FixedRow(components: Component*): Component = Row(components)

  def Row(components: Seq[Component]): Component =
  { val panel = new BoxPanel(Orientation.Horizontal)
    for  { c<-components } panel.contents += c
    panel
  }

  def FixedCol(components: Component*): Component = Col(components)

  def Col(components: Seq[Component]): Component =
  { val panel = new BoxPanel(Orientation.Vertical)
    for  { c<-components } panel.contents += c
    panel
  }

  def Centred(component: Component): Component = FixedRow(Glue.horizontal(), component, Glue.horizontal())

}
