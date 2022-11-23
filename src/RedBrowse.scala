

  import java.awt.{BorderLayout, Dimension, FlowLayout}
  import java.io.IOException
  import java.net.URL
  import javax.swing.{JEditorPane, JFrame, JPanel, JScrollPane}


  object RedBrowse extends Logging.Loggable {

    def main(args: Array[String]): Unit = {
      val url = new URL(args(0))
      createWindow(url)
    }

    private def createWindow(url: URL): Unit = {
      val frame = new JFrame("RedBrowse")
      //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      createUI(frame, url)
      frame.setSize(560, 450)
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
    }

    private def createUI(frame: JFrame, url: URL): Unit = {
      val panel = new JPanel
      val layout = new FlowLayout
      panel.setLayout(layout)
      val jEditorPane = new JEditorPane
      jEditorPane.setEditable(false)
      try jEditorPane.setPage(url)
      catch {
        case e: IOException =>
          jEditorPane.setContentType("text/html")
          jEditorPane.setText("<html>Page not found.</html>")
      }
      val jScrollPane = new JScrollPane(jEditorPane)
      jScrollPane.setPreferredSize(new Dimension(540, 400))
      panel.add(jScrollPane)
      frame.getContentPane.add(panel, BorderLayout.CENTER)
    }
  }
