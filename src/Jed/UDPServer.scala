package Jed

import Jed.Sessions.canQuit

/**
 *
 *  ===Editing Servers===
 *  The main program will use or will act as an "Editing UDPServer" on
 *  the local host, at the port specified by the environment
 *  variable `REDPORT`. If such a variable exists, and
 *  it designates a port number, then when the program is
 *  starting, it either locates an existing server on the designated port,
 *  or sets itself up  as a server reading arguments sent as datagrams
 *  to that port from its clients.
 *
 *  If it has located a server, then it acts as a client, and
 *  sends the program  arguments, one by one to the server.
 *  In this case, path arguments are made absolute, if they are
 *  not already absolute: usually by prefixing them with the
 *  client's current working directory/folder.
 *
 *  If no such environment variable is set, or if it is set to
 *  a non-number, then the program behaves as a stand-alone
 *  editor.
 */
object UDPServer extends Logging.Loggable with ServerInterface {
  private var udpPort: String  = ""

  override def portName: String = s"$udpPort"

  /** When nonEmpty, the port to to which arguments will be sent
   * and the bus that will be used to send them.
   */
  var server: Option[(Int, Red.Bus.Sender)] = None

  /**
   * True iff this Red is a server or a standalone editor.
   */
  var processingLocally = true

  def processLocally(arg: String): Unit =
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

  override def toString: String =
    if (processingLocally || server.isEmpty)
      "Processing arguments locally"
    else
      s"Processing at server $server"

  override def isClient: Boolean = !processingLocally

  override def startServer(): Unit = {
      if (logging) fine(s"serveWith server begin")
      val port = sys.env.get("REDPORT") orElse { sys.props.get("applered.port") }
      port match {
        case None =>
          if (logging) fine("No REDPORT environment variable")
          udpPort = ""
        case Some(portSpec) if portSpec matches "[0-9]+" =>
          server = Some((portSpec.toInt, new Red.Bus.Sender()))
          udpPort = portSpec
          val Some((port, bus)) = server
          try {
            if (logging) fine(s"Probing for Red server at $port (via $bus)")
            bus.send(port, "-probe") match {
              case None =>
                // this program will itself be the server
                // UNLESS IT IS THE PACKAGED APP
                // TODO: excise this branch when OSXApp is fixed
                if (isOSXApp) {
                  processingLocally = false
                  if (logging) warn(s"OS/X Red: needs a server at UDP port $port")
                } else {
                  processingLocally = true
                  // serve here
                  if (logging) warn(s"Starting to serve UDP port $port")
                  startServing(port)
                }

              case Some(_) =>
                // -probe acknowledged: assume there's a server and use it
                if (logging) fine(s"Using UDP server at port $port (via $bus)")
                processingLocally = false
            }
          } catch {
            case exn: Exception =>
              fatal(s"Probing for Red server. ${exn.getLocalizedMessage}")
              sys.exit(1)
          }
        case portSpec =>
          if (logging) error(s"REDPORT environment variable ($portSpec) is not a (port) number")
      }
    }

  /**
   * If this editor is functioning as a server, then
   * this is the receiver on the bus from which it
   * is taking commands; otherwise it is `null`.
   */
  var serverPort: Red.Bus.Receiver = _

  /**
   * Start functioning as an editor server, reading
   * editor arguments from the bus at `port`.
   */
  def startServing(port: Int): Unit = {
    serverPort = new Red.Bus.Receiver(port)
    if (logging) finer(s"Starting Red UDP Server on $port")
    serverPort.start {
      // on receiving an arg from a client, process it here
      arg: String =>
        if (logging) finer(s"Red UDP Server on $port receives $arg")
        processLocally(arg)
    }
    warn(s"Started editor server at $serverPort")
  }

  override def process(arg: String): Unit = {
    if (logging) info(s"process $arg ${if (processingLocally) " locally." else " on server."}")
    if (processingLocally)
      processLocally(arg)
    else
      server match {
        case Some((port, bus)) =>
          // send absolute path for remote processing
          bus.send(
            port,
            if (arg.startsWith("-")) arg else Utils.toAbsolutePath(arg)
          )

        // Only when simulating
        case None =>
          // process locally
          processLocally(arg)
      }
  }

  /** If this editor is functioning as a server,
   * then stop servicing commands arriving on
   * the bus; and close the associated port.
   */
  override def stopServer(): Unit =
    if (serverPort != null) {
      serverPort.stop()
      warn(s"Stopped editor server at $serverPort")
      serverPort = null
    }
}
