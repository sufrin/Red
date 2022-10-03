package Red

import scala.collection.mutable.ListBuffer

/**
 *  A library of primitive classes from which dynamic menus can be constructed.
 */
object Menus extends Logging.Loggable {

  import javax.swing.event.{PopupMenuEvent, PopupMenuListener}

  /**   A Menu whose dynamic content is generated from `titles`
    *   as it is popped up; but only if `titles` has changed since
    *   it was last popped up.
    */
  abstract class LazyDynamicMenu(title: String, titles: => Seq[String])
      extends DynamicMenu(title) {
    var lastTitles: Seq[String] = List()
    def component(title: String): scala.swing.Component
    def content: Seq[scala.swing.Component] = lastTitles.map(component)
    override def popupMenuWillBecomeVisible(): Unit = {
      if (logging) finest(s"Peer popping up: $lastTitles => $titles")
      if (titles != lastTitles) {
        if (logging) finest(s"Refreshing menu: $titles")
        contents.clear()
        lastTitles = titles
        contents ++= content
      }
    }
  }

  /**
   *   A Menu whose content is generated as it pops up
   */
  abstract class DynamicMenu(title: String) extends scala.swing.Menu(title) {
    private val thisDynamicMenu: DynamicMenu = this

    /**
     * @return dynamically-generated content for the menu
     */
    def content: Seq[scala.swing.Component]

    /**
     *  Invoked ''just before'' the menu is about to become
     *  visible.
     */
    def popupMenuWillBecomeVisible(): Unit = {
      val lastContent = contents.toList
      if (logging) finest(s"Peer popping up: $lastContent\n$peer")
      contents.clear()
      contents ++= content
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
     *  '''One the other hand:''' pressing an ordinary button, checkbutton,
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

  /** A ''completely'' dynamic menu that evaluates  `_dynamic` to generate content on popup */
  object DynamicMenu {
    def apply(title: String)(
      _dynamicContent: => Seq[scala.swing.Component]
    ): DynamicMenu = new DynamicMenu(title) {
      def content: Seq[scala.swing.Component] = _dynamicContent
    }
  }

  /**
   *  A menu with a fixed prefix and suffix between which
   *  dynamically-generated content is embedded. The menu
   *  components are regenerated only if `titles` changes its
   *  value.
   */
  abstract class EmbeddedDynamicMenu(title: String, titles: => Seq[String])
    extends Menus.LazyDynamicMenu(title, titles) {
    val prefix, suffix: collection.mutable.Buffer[scala.swing.Component] =
      new ListBuffer[scala.swing.Component]
    override def content: Seq[scala.swing.Component] =
      prefix.toList ++ super.content ++ suffix.toList
  }

}
