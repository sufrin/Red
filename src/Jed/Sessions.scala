package Jed

import java.nio.file.Path


/**
 *  Main program and global coordinator of editing sessions. Exits when all
 *  the editing sessions it has spawned have been closed.
 *
 *  This module keeps track of active (non-closed)
 *  `Red`s by giving each of them a fresh numeric identity, and
 *  keeping a mapping (`activeReds`) of identities to `Red`s.
 *  The identity  of a `Red` has no intrinsic
 *  meaning beyond this; but (as it happens) signifies the order in which
 *  the currently-active sessions were started.
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
 *
 */
object Sessions extends Logging.Loggable {
  // Log warnings and errors
  level = Logging.WARN

  /** Process arguments after locating
   * (or setting up as) a server.
   */
  object argProcessor {
    /** When nonEmpty, the port to to which arguments will be sent
     * and the bus that will be used to send them.
     */
    private var server: Option[(Int, Red.Bus.Sender)] = None
    /**
     * True iff this Red is a server or a standalone editor.
     */
    private var processingLocally = true

    def process(arg: String): Unit = {
      if (logging) info(s"process($arg)")
      if (processingLocally)
        processLocally(arg)
      else
        server match {
          case Some((port, bus)) =>
            // send for remote processing
            bus.send(port, if (arg.startsWith("-")) arg else toAbsolutePath(arg))

          case None =>
            // process locally
            processLocally(arg)
        }
    }

    override def toString: String =
      if (processingLocally || server.isEmpty)
        "Processing arguments locally"
      else
        s"Processing at server $server"

    locally {
      val redPort = sys.env.get("REDPORT")
      val appPort = sys.props.get("applered.port")
      val port    = if (redPort.isEmpty) appPort else redPort
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
                if (sys.props.get("applered.client").nonEmpty)  {
                  if (logging) warn(s"Acting as a client, with server assumed to be at $port (via $bus)")
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
  }

  /**
   *  Start an editing session for each of the paths named on the command line
   *  If a document exists at that path, then the session edits the document
   *  already stored there.
   *
   *  If the no such document exists, the session edits a fresh, blank,
   *  document that is destined to be stored at that path in the filestore.
   *
   *  If an argument is of the form `-l`''module''`=`''level'' then the logging
   *  level for the named module is set to the (named) level.
   *
   *  '''Servers and Server Invocation'''
   *
   *  If (and only if) the environment variable `REDPORT` is set to a port
   *  number, then this is taken to be the port on which an editor server
   *  is listening (or will listen) for paths and other arguments.
   *  If the specified port does not acknowledge a "-probe" argument
   *  "quickly", then this `Red` starts behaving like an editor server
   *  for the specified port.
   *
   *  If an argument takes one of the following forms, then it relates to
   *  probing or stopping an existing server.
   *  {{{
   *    -probe -- Do nothing
   *    -stop  -- Stops the server at `REDPORT` if there is one.
   *    -quit  -- Stops the server at `REDPORT` if there is one,
   *              providing no active editing session remains
   *              open on that server.
   *  }}}
   */


  def main(args_ : Array[String]): Unit = {
    val args=args_ . toList
    if (args.isEmpty)
    { isApp = true
      argProcessor.process(s"New Document")
    }
    else
      for { arg <- args } argProcessor.process(arg)
    if (logging) finer(s"$argProcessor finished")
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


  /** Translate filename arguments to absolute paths
   * to the current working directory in which code invoking
   * the server is running.
   */
  def toAbsolutePath(arg: String): String =
    if (arg.startsWith("-")) arg else {
      import java.nio.file.Path
      Path.of(arg).toAbsolutePath.toString
    }

  /** Yields a new `Red`, editing the document in
   * (or destined to be saved in) the filestore at `fileName`.
   */

  def startSession(fileName: String): Session = {
    if (logging) info(s"startSession($fileName)")
    findRed(fileName) match {
      case None      => new Session(Utils.toPath(fileName), newRedIndex())
      case Some(red) => red
    }
  }

  def startSession(fileName: String, location: String): Session = {
    if (logging) info(s"startSession($fileName, $location)")
    val red = startSession(fileName)
    red.goTo(location)
    red
  }

  private var _redIndex: Int = 0
  private def newRedIndex(): Int = { _redIndex += 1; _redIndex }

  /** Mapping from session identities to active `Red`s */
  val activeReds: collection.mutable.HashMap[Int, Session] =
    new collection.mutable.HashMap[Int, Session]

  def forActiveReds(op: Session=>Unit): Unit =
    for { (_, red) <- activeReds } op(red)

  def closed(red: Session): Unit = {
    activeReds -= red.identity
    info(s"Finished editing ${red.path} at ${Utils.dateString()}")
    forActiveReds {
      case red => info(s"Still editing ${red.path} (#${red.identity})")
    }
    if (activeReds.isEmpty && !Sessions.isApp) System.exit(1)
  }

  var isApp: Boolean = false

  def opened(red: Session): Unit = {
    activeReds += red.identity -> red
  }

  /** Find an active `Red` for a file with the same name, if any. */
  def findRed(fileName: String): Option[Session] = {
    val path = Utils.toPath(fileName)
    activeReds.collectFirst {
      case (_, red) if red.path == path => red
    }
  }

  /**
   * About to rename the session for `oldPath` to a session for `newPath`, because
   * its document has changed path, having been subject to a "Save as...".
   * The corresponding `EditSession`'s path itself has not yet been renamed:
   * it will still be `fromPath`.
   */
  def rename(fromPath: Path, toPath: Path): Unit = {
    if (logging) warn(s"Renaming session $fromPath to $toPath")
    if (logging) forActiveReds {
      case red => info(s"Before rename ${red.path} (#${red.identity})")
    }
      activeReds.collectFirst {
        case (id, red) if red.path == fromPath => red
      } match {
        case None =>
          warn(s"No session at path: $fromPath")
        case Some(red) =>

          if (logging) info(s"Renaming session $fromPath (#${red.identity}) to $toPath")
          activeReds -= red.identity
          activeReds += red.identity -> red
          if (logging) forActiveReds {
            case red => info(s"After rename ${red.path} (#${red.identity})")
          }
      }
  }

  /** Is the ''entire application'' in a state where
   *  quitting without data loss is possible.
   *
   *  First approximation: only if there are no
   *  activeReds sessions that the user is still
   *  interested in.
   */
  def canQuit(): Boolean = {
    forActiveReds (_.close())
    activeReds.isEmpty
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
    if (serverPort!=null) {
      serverPort.stop()
      warn(s"Stopped editor server at $serverPort")
      serverPort = null
    }

  /** Start functioning as an editor server, reading
   * editor arguments from the bus at `port`.
   */
  def startServing(port: Int): Unit =
  { serverPort = new Red.Bus.Receiver(port)
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
