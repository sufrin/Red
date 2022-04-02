package Jed
import Red._

/**
 *  A prototype UI for the cut ring.
 *
 *  This is implemented, opportunistically, as a "normal" UI to
 *  a normal editing session on a document that is kept in sync
 *  with the cut-ring. This is done by rewriting the text of the cut ring
 *  anew into the document whenever the UI is visible and the cut ring
 *  changes.
 *
 */
object CutRingUI extends Logging.Loggable {
    private lazy val doc     = new Document()

    private lazy val session = new EditSession(doc, s"«CUTRING»")

    private lazy val gui     = new UI(session) {
      override def feedback(message: String): Unit = {
        super.feedback(s"$message Cut Ring: ✂ ${CutRing.length}/${CutRing.bound}")
      }

      locally {
        // React to the window opening, or the cut ring changing
        // by refreshing the view.
        CutRing.ringChanged.handleWith {
          case () =>
             refreshIfVisible()
        }

        // Deal with opening and closing of this UI window
        listenTo(top)
        reactions += {
          case swing.event.WindowOpened(_)      => visible = true; refreshIfVisible()
          case swing.event.WindowIconified(_)   => visible = false
          case swing.event.WindowDeiconified(_) => visible = true; refreshIfVisible()
        }
      }
    }

    private var started = false
    private var visible = false

    /** Create (or wake up the existing)
     *  UI for the cut ring. Refresh it
     *  if it is visible.
     */
    def refreshIfVisible(): Unit = {
      if (logging) finest("refreshIfVisible")
      if (!started) {
        started = true
        if (logging) finest("starting")
        gui.start() // The ensuing WindowOpened event causes the visible refresh
      } else
      if (visible) {
        if (logging) finest("CutRingUI is visible")
        doc.clear()
        session.cursor = 0
        session.insert(CutRing.toText)
        session.cursor = 0
        session.notifyHandlers()
        gui.feedback("")
      }
    }

}
