package Jed

/**
 *   Provides editing services directly or indirectly
  *  to `Sessions` by delegating to an implementation
  *  of `ServerInterface`.
  *
  *  ===NB===
  *  Originally there was only a single implementation of `ServerInterface`
  *  but in face of an (OS/X) revision of the accessibility to UDP datagrams
  *  of packaged applications, it seemed to become necessary to
  *  provide at least one other that was not reliant on
  *  receiving instructions via datagrams.
  *
  *  At the time of writing (15 July 2022) this is the subject of
  *  experimental investigation. We built a FIFO srver interface.
  *
  *  Sadly (later that day) it seems that apps are by default truly hamstrung within
  *  newer variants of OS/X, and cannot even read fifos.
  *
  */
object Server extends ServerInterface with Logging.Loggable {

  log.level = Logging.ALL

  val fifo = sys.props.get("applered.fifo") orElse sys.env.get("REDFIFO")

  if (logging) fine(s"Fifo=$fifo")

  def isClient: Boolean = server.isClient

  def portName: String = server.portName

  def startServer(): Unit =
     { server = if (fifo.nonEmpty) FIFOServer else UDPServer
       if (logging) fine(s"server=$server")
       Utils.invokeAndWait { server.startServer() }
     }

  var server: ServerInterface = _

  def process(arg: String): Unit = server.process(arg)
  def stopServer(): Unit = server.stopServer()
}

