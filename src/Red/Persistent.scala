package Red

import java.io.FileOutputStream
import scala.swing.{ButtonGroup, Component, Font, Label, event}

object Persistent {

  lazy val appleRed = java.util.prefs.Preferences.userRoot().node("/AppleRed")

  def showState(): Unit = {
    appleRed.exportSubtree(new FileOutputStream("/dev/tty"))
  }

  def clearState(): Unit = {
    for { name <- appleRed.childrenNames() } appleRed.node(name).removeNode()
    appleRed.flush()
  }

  def clearFeatures(): Unit = {
    appleRed.node("Features").removeNode()
    appleRed.flush()
  }


  def sync(): Unit = { appleRed.sync() }

  /**
   * A persistent feature is in-effect a typed variable that has a presence in the persistent store.
   * On construction the value of the variable is taken from the persistent store if it is there,
   * otherwise it is set to the value of the `_value` parameter.
   *
   * @param key key in the persistence store
   * @param path path to node in the persistence store where the key resides
   * @param _value initial default value of the feature.
   * @param title (if non-"") title on menus, etc.
   * @tparam T the type of the feature
   */

  abstract class Feature[T](val key: String, val path: String, var _value: T, title: String = "") { feature =>
    def fromString(s: String): T
    def toString(t: T): String = t.toString
    def profileString: String = (s"$path.$key=${toString(_value)}")

    /** If the feature may have only certain values, then these are the values. */
    def choices: Seq[T] = Nil

    class Group(choices: Seq[T]) extends ButtonGroup {
      val items: Seq[swing.CheckBox] = for { choice <- choices } yield new swing.CheckBox(feature.toString(choice)) {
        font = Utils.menuButtonFont
        reactions += { case event.ButtonClicked(_) =>
          if (selected) {
            feature.value = choice
          }
        }
        selected = feature.value == choice
      }

      locally {
        for { item <- items } buttons += item
      }

      def setValue(newValue: T): Unit = {
        for { bix <- 0 until choices.length } { items(bix).selected = choices(bix)==newValue }
      }
    }

    var group: Option[Group] = None

    // Read the initial value from the persistent store, if it's there, else put it there
    locally {
      val node = appleRed.node(path)
      if (node.keys.contains(key)) _value = fromString(node.get(key, toString(_value)))
      sync()
    }

    /** Synchronize this feature to the store */
    def sync(): Unit = {
      appleRed.node(path).put(key, toString(_value))
      appleRed.node(path).sync()
      appleRed.sync()
    }

    def value: T = _value

    def value_=(newValue: T): Unit = {
      _value = newValue
      if (group.nonEmpty) group.get.setValue(newValue)
      sync()
    }

    def setValue(newValue: T): Unit = value=newValue

    override def toString: String = (s"Feature($key,$path,${_value})")

    def menuItem: Component = RadioItem(this.choices)

    def layout(choices: Seq[T]): (Boolean, Int) = (true, 0)

    def RadioItem(choices: Seq[T]): Component = {
      group = Some(new Group(choices))
      val (vertical, cols) = layout(choices)
      if (vertical)
      Buttons.FixedCol(
        Buttons.Centred(new Label(if (title=="") key else title) { font=Utils.menuButtonFont }),
        Buttons.Row(List(Glue.quad(20))++group.get.items++List(Glue.quad(20)))
      )
      else {
        val lab = new Label(if (title=="") key else title) { font=Utils.menuButtonFont }
        Buttons.Row(List(Glue.quad(20), lab, Glue.quad(20, plus=10000))++group.get.items++List(Glue.quad(20)))
      }
    }
  }

  trait AnyFeature {
    val key:           String
    def menuItem:      Component
    def profileString: String
    val path:          String
  }

  class BoolFeature(_key: String, _path: String, _default: Boolean, _title: String = "") extends Feature[Boolean](_key, _path, _default, _title) with AnyFeature  {
    def fromString(rep: String): Boolean = rep match {
      case "true" => true
      case "false" => false
    }

    override def menuItem: Component = {
      val label = new swing.CheckBox(if (_title == "") key else _title) {
        font = Utils.menuButtonFont
        selected = value
        reactions += { case event.ButtonClicked(_) =>
          value = selected
        }
      }
      Buttons.FixedRow(label, Glue.horizontal())
    }
  }

  class StringFeature(_key: String, _path: String, _default: String, _title: String="") extends Feature[String](_key, _path, _default, _title) with AnyFeature { feature =>
    def fromString(rep: String): String = rep
  }

  class IntFeature(_key: String, _path: String, _default: Int, _title: String="") extends Feature[Int](_key, _path, _default, _title) with AnyFeature {
    def fromString(rep: String): Int = rep.toInt
  }

  class LongFeature(_key: String, _path: String, _default: Long, _title: String="") extends Feature[Long](_key, _path, _default, _title) with AnyFeature  {
    def fromString(rep: String): Long = rep.toInt
  }

  class FontFeature(_key: String, _path: String, _default: Font, _title: String="") extends Feature[Font](_key, _path, _default, _title) with AnyFeature {
    def fromString(fontName: String): Font = Utils.mkFont(fontName)
    override def toString(f: Font): String = s"${f.getFontName()}/=${f.getStyle}/${f.getSize}"
  }
}
