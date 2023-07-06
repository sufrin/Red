package Red

import scala.collection.mutable.ListBuffer

/**
 *  A library of primitive classes from which dynamic menus can be constructed.
 */
object Menus extends Logging.Loggable {

  import javax.swing.event.{PopupMenuEvent, PopupMenuListener}

  /**   A Menu whose dynamic content is generated from `descriptors`
    *   as it is popped up; but only if `descriptors` has changed since
    *   it was last popped up.
    */
  abstract class LazyDynamicMenu(title: String, descriptors: => Seq[String])
      extends DynamicMenu(title) {

    private var lastdescriptors: Seq[String] = List()

    protected def dynamicContents: Seq[scala.swing.Component] = lastdescriptors.map(component)

    def component(descriptor: String): scala.swing.Component

    override def popupMenuWillBecomeVisible(): Unit = {
      if (logging) finest(s"Peer popping up: $lastdescriptors => $descriptors")
      if (descriptors != lastdescriptors) {
        if (logging) finest(s"Refreshing menu: $descriptors")
        contents.clear()
        lastdescriptors = descriptors
        contents ++= dynamicContents
      }
    }
  }

  /**
   *   A Menu whose dynamic content is generated as it pops up
   */
  abstract class DynamicMenu(title: String) extends scala.swing.Menu(title) { thisDynamicMenu =>
    /**
     * @return dynamically-generated content for the menu
     */
    protected def dynamicContents: Seq[scala.swing.Component]

    /**
     *  Invoked ''just before'' the menu is about to become
     *  visible.
     */
    def popupMenuWillBecomeVisible(): Unit = {
      val lastContent = contents.toList
      if (logging) finest(s"Peer popping up: $lastContent\n$peer")
      contents.clear()      // clear the existing menu elements
      contents ++= dynamicContents  // add the current menu elements
    }

    /**
     *  Invoked ''just before'' the menu is about to become
     *  invisible.
     *
     *  '''Beware:''' there is empirical evidence that
     *  clicking on a normal menuItem (or one of its descendants)
     *  causes this to be invoked ''before'' any action invoked by
     *  the menu item.
     *
     *  '''On the other hand:''' pressing an ordinary button, checkbutton,
     *  etc doesn't cause a menu it is on to become invisible: that
     *  only happens when the cursor strays outside the menu box.
     *
     *  TODO: check the warning
     */
    def popupMenuWillBecomeInvisible(): Unit = {
      val lastContent = contents.toList
      if (logging) finest(s"Peer popping down: $lastContent\n$peer")
    }

    def popupMenuCanceled(): Unit = {}

    locally {
      // WARNING: PopupMenuListener doesn't work on Mac in the top bar with native look and feel
      val peerPopup = peer.getPopupMenu
      peerPopup.addPopupMenuListener(new PopupMenuListener() {
        override def popupMenuWillBecomeVisible(e: PopupMenuEvent): Unit =
          thisDynamicMenu.popupMenuWillBecomeVisible()
        override def popupMenuWillBecomeInvisible(e: PopupMenuEvent): Unit =
          thisDynamicMenu.popupMenuWillBecomeInvisible()
        override def popupMenuCanceled(e: PopupMenuEvent): Unit =
          thisDynamicMenu.popupMenuCanceled()
      })
    }
  }

  /** A ''completely'' dynamic menu that evaluates  `_dynamic` to generate dynamicContents on popup */
  object DynamicMenu {
    def apply(title: String)(
      _dynamicContent: => Seq[scala.swing.Component]
    ): DynamicMenu = new DynamicMenu(title) {
      def dynamicContents: Seq[scala.swing.Component] = _dynamicContent
    }
  }

  /**
   *  A menu with a fixed prefix and suffix between which
   *  dynamically-generated components are embedded. The menu
   *  components are regenerated only if `descriptors` changes its
   *  value.
   */
  abstract class EmbeddedDynamicMenu(title: String, descriptors: => Seq[String])
    extends Menus.LazyDynamicMenu(title, descriptors) {
    val prefix, suffix: collection.mutable.Buffer[scala.swing.Component] =
      new ListBuffer[scala.swing.Component]
    override def dynamicContents: Seq[scala.swing.Component] =
      prefix.toList ++ super.dynamicContents ++ suffix.toList
  }

}
