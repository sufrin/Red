

  import java.awt.{BorderLayout, Dimension}
  import java.io.IOException
  import java.net.URL
  import javax.swing._
  import javax.swing.text.Document
  import javax.swing.text.html.HTMLDocument
  import scala.swing.Font


  object RedBrowse extends Logging.Loggable {

    def main(args: Array[String]): Unit = {
      val url = new URL(s"file:${args(0)}")
      createWindow(url)
    }

    private def createWindow(url: URL): Unit = {
      val frame = new JFrame("RedBrowse")
      //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      createUI(frame, url)
      frame.setSize(800, 800)
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
    }

    private def createUI(frame: JFrame, url: URL): Unit = {
      lazy val jEditorPane = new JEditorPane { parent =>
        import javax.swing.JEditorPane
        import javax.swing.event.{HyperlinkEvent, HyperlinkListener}
        override def updateUI(): Unit = {
            super.updateUI()
            putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true)
            val font = getFont()
            setFont(font.deriveFont(18.0f))
        }
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10)) // t, l, b, r
        addHyperlinkListener(new HyperlinkListener() {
        def hyperlinkUpdate(ev: HyperlinkEvent): Unit = {
          if (logging) info(s"${ev.getDescription()} ${ev.getEventType} ${ev.getClass}")
          if (ev.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            val ref = ev.getDescription()
            val evURL = ev.getURL()
            val localRef = ref != null && ref.startsWith("#")
            if (logging) info(if (localRef) s"Local Hyperlinkevent $ref (URL: $evURL)" else s"Hyperlinkevent $ref (URL: $evURL) ")
            if (false && localRef) {
                val pane = ev.getSource.asInstanceOf[JEditorPane]
                if (logging) info(s"Scrolling $ref")
                val doc:HTMLDocument = pane.getDocument().asInstanceOf[HTMLDocument]
                //doc.processHTMLFrameHyperlinkEvent(ev.asInstanceOf[javax.swing.event.HTMLFrameHyperlinkEvent])
                SwingUtilities.invokeLater(new Runnable() {
                  def run(): Unit = {
                    parent.scrollToReference(ref.substring(1))
                    if (logging) info(s"Scrolled $parent")// parent.set
                  }
                })
            }
            else {
                val newURL = ev.getURL()
                if (logging) info(s"SetPage($url => ${ev.getURL()})")
                val doc: HTMLDocument = parent.getDocument().asInstanceOf[HTMLDocument]
                doc.putProperty(Document.StreamDescriptionProperty, null);
                setPage(newURL)
            }
          }
        }
      })
      }
      jEditorPane.setEditable(false)
      try {
        jEditorPane.setPage(url)
      }
      catch {
        case e: IOException =>
          jEditorPane.setContentType("text/html")
          jEditorPane.setText("<html>Page not found.</html>")
      }
      lazy val jScrollPane = new JScrollPane(jEditorPane)
      jScrollPane.setPreferredSize(new Dimension(720, 500))
      val panel  = new JPanel()
      val bar    = new JToolBar() {
        locally {
          setFont(new Font("Monospaced", 0,  20))
        }
      }
      val layout = new BorderLayout()
      val bigger  = scala.swing.Button("+") {
        val pf = jEditorPane.getFont()
        val newSize : Float = 1.1f*pf.getSize2D
        jEditorPane.setFont(pf.deriveFont(newSize*1.2f))
      }
      val smaller = scala.swing.Button("-") {
        val pf = jEditorPane.getFont()
        val newSize : Float = pf.getSize2D*0.9f
        jEditorPane.setFont(pf.deriveFont(newSize))
      }
      smaller.font=(bar.getFont())
      bigger.font=(bar.getFont())
      bar.add(smaller.peer)
      bar.add(bigger.peer)
      panel.setLayout(layout)
      panel.add("North", bar)
      panel.add("Center", jScrollPane)
      frame.getContentPane.add(panel, BorderLayout.CENTER)
    }
  }
