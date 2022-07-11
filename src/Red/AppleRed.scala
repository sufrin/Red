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
    withDesktop { Jed.Sessions.main(args) }

  def withDesktop(body: => Unit): Unit = {
    val desk: Desktop = Desktop.getDesktop

    // This property is set for the OS/X app
    sys.props.get("applered.port") match {
      case None =>
      case Some(port) =>
        import scala.swing._
        val ffont   = Jed.Utils.defaultFont
        val redLine = "\uf8ff Red \uf8ff"
        val frame = new swing.MainFrame() {
          val panel = new BoxPanel(Orientation.Vertical) {
            val user = System.getProperty("user.name", "<no user>")
            val label = new Label(
              s"<html><center><b>$redLine</b></center><center><b>$user</b></center><center><b>($port)</b></center></html>"
            ) {
              border = javax.swing.BorderFactory.createEtchedBorder()
              horizontalAlignment = Alignment.Center
            }
            contents += label
            if (false) {
              // TODO: The app doesn't respond to this button. Why?
              contents += Jed.Utils.Button("New", "Start editing a new file") {
                Jed.Sessions.startSession(s"AppleRed+${Jed.Utils.dateString()}")
              }
            }
            iconImage = Jed.Utils.redImage
            border    = javax.swing.BorderFactory.createEtchedBorder()
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
            // val names = for {f <- files} yield f.toString
            // open a new session for each open request
            // for {f <- names} Red(f)
            // main(names)
            while (files.hasNext) {
              val file     = files.next()
              val fileName = file.getAbsolutePath
              println(s"Opening $fileName")
              if (fileName != null) Jed.Sessions.argProcessor.process(fileName)
              else Jed.Sessions.argProcessor.process("UNTITLED")
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

