package Jed

import Jed.Sessions.canQuit

/**  A main object that starts a server listening on the FIFO denoted
  *  by the environment variable `REDFIFO` or the property `applered.fifo`.
  *
  *  ===NB===
  *  This was built to get around the differential reachability of
  *  UDP packets: an OS/X app simply does not receive UDP packets,
  *  so when it's started it does not turn itself into a server.
  *
  *  ===Editing Servers===
  *  The main program will use or will act as an "Editing Server" on
  *  the local host, at the port specified by the environment
  *  variable `REDPORT`. If such a variable exists, and
  *  it designates a port number, then when the program is
  *  starting, it either locates an existing server on the designated port,
  *  or sets itself up  as a server reading arguments sent as datagrams
  *  to that port from its clients.
  *
  *  If it has located a server, then it acts as a client, and
  *  sends the program  arguments, one by one to the server.
  *  In this case, path arguments are made absolute if they are
  *  not already absolute by prefixing them with the
  *  client's current working directory/folder.
  *
  *  If no such environment variable is set, or if it is set to
  *  a non-number, then this program behaves as a stand-alone
  *  editor.
  */
object Server extends Logging.Loggable {

  /** When nonEmpty, the port to to which arguments will be sent
    * and the bus that will be used to send them.
    */
  var server: Option[(Int, Red.Bus.Sender)] = None

  /**
   * True iff this Red is a server or a standalone editor.
   */
  var processingLocally = true

  def process(arg: String): Unit = {
    if (logging) info(s"process($arg)")
    if (processingLocally)
      processLocally(arg)
    else
      server match {
        case Some((port, bus)) =>
          // send for remote processing
          bus.send(
            port,
            if (arg.startsWith("-")) arg else Utils.toAbsolutePath(arg)
          )

        case None =>
          // process locally
          processLocally(arg)
      }
  }

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
        if (canQuit()) {
          stopServer()
          System.exit(0)
        }
      case s"-$unknown" =>
        warn(s"$arg is an unknown switch")
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

  locally {
    val redPort = sys.env.get("REDPORT")
    val appPort = sys.props.get("applered.port")
    val port = if (redPort.isEmpty) appPort else redPort
    port match {
      case None =>
        if (logging) fine("No REDPORT environment variable")
      case Some(portSpec) if (portSpec matches "[0-9]+") =>
        server = Some((portSpec.toInt, new Red.Bus.Sender()))
        if (logging) fine(s"Sending -probe to $server")
        val Some((port, bus)) = server
        try {
          // when packaged as an /app/ the program no longer starts itself as a server
          // in the absence of a response to -probe
          // I don't understand why this is.
          bus.send(port, "-probe") match {
            case None =>
              // -probe was not acknowledged: assume no server on port
              if (sys.props.get("applered.client").nonEmpty) {
                if (logging)
                  warn(
                    s"Acting as a client, with server assumed to be at $port (via $bus)"
                  )
                processingLocally = false
              } else {
                // serve here
                startServing(port)
                processingLocally = true
              }
            case Some(_) =>
              // -probe acknowledged: assume there's a server, process using the server
              if (logging) fine(s"Server is at $port (via $bus)")
              processingLocally = false
          }
        } catch {
          case exn: Exception =>
            fatal(s"Probing for Red server. ${exn.getLocalizedMessage}")
            sys.exit(1)
        }
      case _ =>
        if (logging) fine("REDPORT environment variable is not a (port) number")
    }
  }

  /** If this editor is functioning as a server, then
    * this is the receiver on the bus from which it
    * is taking commands; otherwise it is `null`.
    */
  var serverPort: Red.Bus.Receiver = _

  /** If this editor is functioning as a server,
    * then stop servicing commands arriving on
    * the bus; and close the associated port.
    */
  def stopServer(): Unit =
    if (serverPort != null) {
      serverPort.stop()
      warn(s"Stopped editor server at $serverPort")
      serverPort = null
    }

  /** Start functioning as an editor server, reading
    * editor arguments from the bus at `port`.
    */
  def startServing(port: Int): Unit = {
    serverPort = new Red.Bus.Receiver(port)
    if (logging) finer(s"Red Server on $port")
    serverPort.start {
      // on receiving an arg from a client process it here
      case arg: String =>
        if (logging) finer(s"Red $port <- $arg")
        processLocally(arg)
    }
    warn(s"Started editor server at $serverPort")
  }

}
