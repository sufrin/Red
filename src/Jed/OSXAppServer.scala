package Jed

/**
 *   Service implementation designed to function with the OS/X single-instance model rather than
 *   communicating client/service via some sort of comms technology. The essential distinction is that arguments
 *   are processed locally.
 */
object OSXAppServer extends Logging.Loggable with ServerInterface {

  import Jed.Sessions.canQuit

    private var udpPort: String  = "AppleRed.app"

    override def portName: String = s"$udpPort"

    def processLocally(arg: String): Unit = {
      if (logging) fine(s"$this: $arg")
      arg match {
        case "-probe" =>
          if (logging) fine(s"probed")
        case s"-l${module}=${level}" =>
          Logging.update(module, level)
          Logging.Default.info(s"$module=$level")
        case s"-stop" =>
          stopServer()
        case s"-quit" =>
          if (canQuit) {
            stopServer()
            System.exit(0)
          }
        case s"-$unk" =>
          warn(s"-$unk is an unknown switch")
        case s"$path@$location" =>
          Sessions.startSession(path, location)
        case path =>
          Sessions.startSession(path)
      }
    }

  override def toString: String =
      s"AppleRed app (processing locally)"

    override def isClient: Boolean = false

    override def startServer(): Unit = {
      if (logging) fine(s"$this started")
    }


    override def process(arg: String): Unit = {
      if (logging) info(s"process $arg")
        processLocally(if (arg.startsWith("-")) arg else Utils.toAbsolutePath(arg))
    }

    override def stopServer(): Unit =
        warn(s"Stopped editor server $this")

  }
