package Red
import java.awt.Desktop
import java.awt.desktop._

/** Apple Application root for Red. The additional functionality this
 * currently provides is to define a `QuitHandler` to respond to `Command-Q` so
 * as to avoid uncontrolled termination of the entire program, which leads to
 * loss of all unsaved edits. This is replaced by the controlled "polling" of
 * open editing sessions to see what the user's intent for them is.
 */
object AppleRed extends Logging.Loggable {

  def main(args: Array[String]): Unit =
    //SwingUtilities.invokeLater { new Runnable { def run(): Unit = withDesktop{ Jed.Sessions.main(args) } } }
    withDesktop {
      Jed.Sessions.main(args)
    }

  def withDesktop(body: => Unit): Unit = {
    val desk: Desktop = Desktop.getDesktop

    // This property is set for the OS/X app
    sys.props.get("applered.port") match {
      case None =>
        // not an OS/X app; so we need no main window
        appMainFrame("Linux")
      case Some(port) =>
        // set up appMainFrame
        appMainFrame(s"On ${port.toString}")
    }

    def appMainFrame(port: String): swing.Frame = {
      import scala.swing._
      val ffont = Jed.Utils.defaultFont
      val redLine = "\uf8ff Red \uf8ff"
      val frame = new MainFrame() {
        val panel = new BoxPanel(Orientation.Vertical) {
          val user = System.getProperty("user.name", "<no user>")
          val client = if (sys.props.get("applered.client").nonEmpty) "<center><b>[Client]</b></center>" else ""
          val label = new Label(
            s"<html><center><b>$redLine</b></center><center><b>$user</b></center><center><b>($port)</b></center>$client</html>"
          ) {
            border = javax.swing.BorderFactory.createEtchedBorder()
            horizontalAlignment = Alignment.Center
          }
          contents += label
          if (true) {
            // TODO: The app doesn't respond to this button. Why?
            contents += Jed.Utils.Button("New", "Start editing a new file") {
              Jed.Server.process(s"AppleRed+${Jed.Utils.dateString()}")
            }
          }
          iconImage = Jed.Utils.redImage
          border = javax.swing.BorderFactory.createEtchedBorder()
        }
        // Frame
        contents = panel
        visible = true
        title = s" $redLine "
        peer.setLocationRelativeTo(null)
        iconify()
        peer.setResizable(false)
        peer.setDefaultCloseOperation(
          javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
        )
      }
      frame
    }

    // These facilities are not available on Linux
    try {
      desk.disableSuddenTermination()
      desk.setQuitHandler {
        new QuitHandler() {
          def handleQuitRequestWith(qe: QuitEvent, qr: QuitResponse): Unit = {
            fine(s"QuitEvent(${qe.getSource})")
            if (Jed.Sessions.canQuit())
              qr.performQuit()
            else
              qr.cancelQuit()
          }
        }
      }

      desk.setOpenFileHandler(
        new OpenFilesHandler {
          override def openFiles(e: OpenFilesEvent): Unit = {
            val files = e.getFiles().iterator()
            while (files.hasNext) {
              val file     = files.next()
              val fileName = file.getAbsolutePath
              info(s"Opening $fileName")
              if (fileName != null)
                Jed.Server.process(fileName)
              else
                Jed.Server.process("UNTITLED-FROM-OPENFILES-HAndler")
            }
          }
        }
      )

      body
    } catch {
      case exn: Exception => ()
    }

  }
}

