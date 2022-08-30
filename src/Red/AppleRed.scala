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
 * means that ''the packaged app cannot act as a server'', for it cannot
 * receive instructions from clients.
 *
 * 24/July/2022 the packaged app cannot read Unix domain sockets either,
 * without specific permissions are given!
 *
 * TODO: investigate UDP-receive permissions
 *
 */
object AppleRed extends Logging.Loggable {

  def main(args: Array[String]): Unit = {
    val logStream =
        sys.props.get("applered.log") orElse { sys.env.get("REDLOG") } match {
          case None       => null
          case Some(path) => Logging.logStream(Utils.expandHome(path), mustExist = true, append = true)
        }
    Logging.withConsole(logStream) {
      Logging.Default.info(s"\n**********\nAppleRed starting at ${Utils.dateString()}\n**********")
      Jed.Server.startServer()

      withDesktop {
        if (logging) fine(s"Server interface started")
        if (Jed.Server.isOSXApp || Jed.Server.isServer) {
          scala.swing.Swing.onEDTWait { establishMainWindowFrame(Jed.Server.portName) }
        }
        if (logging) fine(s"Main window $mainWindowFrame")
        if (Jed.Server.isServer && args.isEmpty) {
          Jed.Sessions.exitOnLastClose = false
        }
        if (logging)
          fine(s"Processing args: ${args.mkString(", ")}")
        for {arg <- args} Jed.Server.process(arg)
        if (logging) fine(s"(args processed) client=${Jed.Server.isClient} noExitOnLastClose=${Jed.Server.isOSXApp}")
      }
    }
  }

  def wherePossible(body: => Unit): Unit = try { body } catch { case exn: UnsupportedOperationException => {} }

  def withDesktop(body: => Unit): Unit = {
    val desk: Desktop = Desktop.getDesktop

    // One or more of these facilities are not available on Linux

      wherePossible { desk.disableSuddenTermination() }
      wherePossible {
        desk.setQuitHandler {
            new QuitHandler() {
              def handleQuitRequestWith(qe: QuitEvent, qr: QuitResponse): Unit = {
                fine(s"QuitEvent(${qe.getSource})")
                if (Jed.Sessions.canQuit)
                  qr.performQuit()
                else
                  qr.cancelQuit()
              }
            }
      }
    }

    wherePossible {
      desk.setOpenFileHandler(
        new OpenFilesHandler {
          override def openFiles(e: OpenFilesEvent): Unit = {
            val files = e.getFiles.iterator()
            while (files.hasNext) {
              val file = files.next()
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
    }

    // AND FINALLY
    body
  }

  private var mainWindowFrame: Frame = _

  /** Set up the window that's the representative for the app as a whole  */
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

    val redLine = " « Red » " // "\uf8ff Red \uf8ff"
    val mainFrame: MainFrame = new MainFrame() {
        private val panel   = new BoxPanel(Orientation.Vertical) {
        private val user    = System.getProperty("user.name", "<no user>")
        private val role    =
          if (Jed.Server.isOSXApp)
             // the OS/X packaged app has to be a client, for the time being
             s"Client (of $port)"
          else
          if (Jed.Server.isServer && port!="")
             s"Serving $port"
          else
             "(Standalone)"


        private val labels  = new BoxPanel(Orientation.Vertical) {
          contents += Lab(redLine)
          contents += Lab(user)
          contents += Lab(role)
        }

        private val buttons = new BoxPanel(Orientation.Horizontal) {
          contents += But("Fresh", "Start editing a new document") {
            Jed.Server.process(Utils.freshDocumentName())
          }

          /*
           * Experiments using this button seem to indicate that a packaged OS/X app may have no more than one
           * top-level window.
           *
           * Symptoms:
           *   1. On the non-packaged program the session starts, and logs, as expected
           *   2. On the packaged program there is no log of the session starting
           *      and apparently no system log of a permission failure
           *
           *   But the investigation isn't over, for I ask myself why Jape can
           *   possibly work on this machine under these circumstances!
           */
          if (false && Jed.Server.isOSXApp) {
            contents += But("Local", "Start editing a new document locally") {
              Jed.Sessions.startSession(Utils.freshDocumentName())
            }
          }


          // This nonsense is while the OS/X packaged app can't communicate

          if (!Jed.Server.isOSXApp)
            contents += But("Quit", "Quit this server") {
              if (Jed.Sessions.canQuit) sys.exit(0)
            }

          if (false && !Jed.Server.isOSXApp)
            contents += But("Serve", "Start a new appleredserver") {
            Utils.startRedServerProcess(Jed.Server.portName)
          }

        }
        contents  += labels
        contents  += buttons
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
      peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
      override def closeOperation(): Unit = { if (Jed.Sessions.canQuit) sys.exit(0) }
    }
    mainWindowFrame=mainFrame
  }

}

