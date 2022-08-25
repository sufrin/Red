package Jed

object Menus extends Logging.Loggable {

  import javax.swing.event.{PopupMenuEvent, PopupMenuListener}

  /**
   *   A Menu whose dynamic content is generated from `titles`
   *   as it is popped up; but only if `titles` has changed since
   *   it was last popped up.
   */
  abstract class LazyDynamicMenu(title: String, titles: => Seq[String]) extends DynamicMenu(title) {
    var lastTitles: Seq[String] = List()
    def component(title: String): scala.swing.Component
    def content: Seq[scala.swing.Component] = lastTitles.map(component)

    override def popupMenuWillBecomeVisible(): Unit = {
      if (logging) finest(s"Peer popping up: $lastTitles => $titles")
      if (titles!=lastTitles) {
        if (logging) finest(s"Refreshing menu: $titles")
        contents.clear()
        lastTitles=titles
        contents ++= content
      }
    }
  }

  /**
   *   A Menu whose content is completely generated as it pops up
   */
  abstract class DynamicMenu(title: String) extends scala.swing.Menu(title) {
    val thisDynamicMenu: DynamicMenu = this

    def content: Seq[scala.swing.Component]

    def popupMenuWillBecomeVisible(): Unit = {
      val lastContent = contents.toList
      if (logging) finest(s"Peer popping up: $lastContent\n$peer")
      contents.clear()
      contents ++= content
    }

    def popupMenuWillBecomeInvisible(): Unit = {
      val lastContent = contents.toList
      if (logging) finest(s"Peer popping down: $lastContent\n$peer")
    }

    def popupMenuCanceled(): Unit = {}

    locally {
      // WARNING: PopupMenuListener doesn't work on Mac in the top bar with native look and feel
      val peerPopup = peer.getPopupMenu
      peerPopup.addPopupMenuListener(new PopupMenuListener() {
        override def popupMenuWillBecomeVisible(e: PopupMenuEvent): Unit = thisDynamicMenu.popupMenuWillBecomeVisible()
        override def popupMenuWillBecomeInvisible(e: PopupMenuEvent): Unit = thisDynamicMenu.popupMenuWillBecomeInvisible()
        override def popupMenuCanceled(e: PopupMenuEvent): Unit = thisDynamicMenu.popupMenuCanceled()
      })
    }
  }

}
