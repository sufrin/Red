package Red
import Jed.Utils

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

  def main(args: Array[String]): Unit =
     { Jed.Server.startServer()
       withDesktop {
         if (logging) fine(s"Server interface started")
         if (Jed.Server.isApp || Jed.Server.isServer) establishMainWindowFrame(Jed.Server.portName)
         if (logging) fine(s"Main window ${mainWindowFrame}")
         if (Jed.Server.isServer && args.isEmpty) {
           Jed.Sessions.exitOnLastClose = false
         }
         if (logging) fine(s"Processing args: $args")
         for  { arg <- args } Jed.Server.process(arg)
         if (logging) fine(s"(args processed) client=${Jed.Server.isClient} noExitOnLastClose=${Jed.Server.isApp}")
      }
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

    /** @return a centred label */
    def Lab(title: String): Component = {
      new BoxPanel(Orientation.Horizontal) {
        contents += Red.Glue.horizontal()
        contents += new Label(title) // { font = Utils.buttonFont }
        contents += Red.Glue.horizontal()
      }
    }

    /** @return a centred button */
    def But(title: String, tip: String="")(act: => Unit): Component = {
      new BoxPanel(Orientation.Horizontal) {
        contents += Red.Glue.horizontal()
        contents += { val b = Button(title) { act }; b.tooltip=tip; b }
        contents += Red.Glue.horizontal()
      }
    }

    val redLine = "\uf8ff Red \uf8ff"
    val mainFrame = new MainFrame() {
        val panel = new BoxPanel(Orientation.Vertical) {
        val user = System.getProperty("user.name", "<no user>")
        val client = if (sys.props.get("applered.app").nonEmpty) "<center><b>[Client]</b></center>" else ""
        val labels  = new BoxPanel(Orientation.Vertical) {
          contents += Lab(redLine)
          contents += Lab(user)
          contents += Lab(port)
        }
        val buttons = new BoxPanel(Orientation.Horizontal) {
          contents += But("Fresh", "Start editing a new document locally") {
            Jed.Server.process(Utils.freshDocumentName())
          }

          contents += But("Quit", "Quit this server") {
            if (Jed.Sessions.canQuit()) sys.exit(0)
          }

          contents += But("Serve", "Start a new appleredserver") {
            Utils.startServer(Jed.Server.portName)
          }

        }
        contents  += labels
        contents  += buttons
        iconImage = Jed.Utils.redImage
        //border = javax.swing.BorderFactory.createEtchedBorder()
      }
      // Frame
      contents = panel
      visible = true
      title = s" $redLine "
      peer.setLocationRelativeTo(null)
      iconify()
      peer.setResizable(false)
      peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
      override def closeOperation(): Unit = { if (Jed.Sessions.canQuit()) sys.exit(0) }
    }
    mainWindowFrame=mainFrame
  }

}

