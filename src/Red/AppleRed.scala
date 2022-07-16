package Red
import java.awt.Desktop
import java.awt.desktop._
import scala.swing.Frame

/**
 * ==Red Application Main Program==
 *
 * ===All Operating systems===
 * If a `REDPORT` environment variable denotes a local (UDP) port
 * then the main program establishes communications with a currently-running
 * Red server, or in the absence of such a server sets itself up as one.
 * It then establishes editing sessions for all its (file) arguments by
 * passing them to the server (which may be itself).
 *
 * If there is no such environment variable then the main program
 * establishes editing sessions for all its (file) arguments, and terminates
 * when all sessions have terminated.
 *
 * ===OS/X additional functionality===
 * Defines a `QuitHandler` to respond to `Command-Q` so as to avoid
 * uncontrolled termination of the entire program -- which leads to loss
 * of all unsaved edits.
 * This behaviour is replaced by controlled "polling" of currently
 * open editing sessions to see what the user's intent for them is.
 *
 * === OS/X Packaged App ===
 * Keeps the packaged app alive by retaining its main window even when it is (only)
 * acting as a client (see below). This permits its dock icon to be used
 * as the target for file drops providing it can locate a running server.
 *
 * ====Explanation (July 2022) ====
 * Recent security changes in OS/X now make it impossible for the packaged app
 * to receive UDP packets or (apparently) to read from FIFOs unless specific
 * permission is given to the app (by means I have not yet explored). This
 * means that the packaged app cannot act as a server'', for it cannot
 * receive instructions from clients.
 *
 * TODO: investigate UDP-receive permissions
 *
 */
object AppleRed extends Logging.Loggable {
  log.level = Logging.ALL

  def main(args: Array[String]): Unit =
    withDesktop {
      Jed.Server.startServer()
      if (logging) fine(s"Server interface started")
      for  { arg <- args } Jed.Server.process(arg)
      if (logging) fine(s"(args processed) client=${Jed.Server.isClient} isApp=${Jed.Server.isApp}")
      // Close/dispose of the main window unless we are an OS/X (client) app
      // The retained window keeps the forwarding server alive
      if (Jed.Server.isClient && !Jed.Server.isApp)
        mainWindowFrame.close()
    }

  def withDesktop(body: => Unit): Unit = {
    val desk: Desktop = Desktop.getDesktop

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
                Jed.Server.process("UNTITLED-FROM-OPENFILES-Handler")
            }
          }
        }
      )

      body
    } catch {
      case exn: Exception => ()
    }
  }

  private var mainWindowFrame: Frame = _

  def establishMainWindowFrame(port: String): Unit = {
    import scala.swing._
    val ffont = Jed.Utils.defaultFont
    val redLine = "\uf8ff Red \uf8ff"
    val mainFrame = new MainFrame() {
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
    mainWindowFrame=mainFrame
  }

}

