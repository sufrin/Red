package Red


import javax.swing._
import scala.swing.{Component, Orientation, Oriented}
/**
 * A bar that can be used a separator, most commonly
 * for use in menus.
 *
 * @see javax.swing.JSeparator
 */
  class Separator(o: Orientation.Value)
        extends Component
          with  Oriented.Wrapper
  {
    override lazy val peer: JSeparator = new JSeparator(o.id) with SuperMixin
    def this() = this(Orientation.Horizontal)
  }

