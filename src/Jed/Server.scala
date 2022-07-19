package Jed

/**
 *   Provides editing services directly or indirectly
  *  to `Sessions` by delegating to an implementation
  *  of `ServerInterface`.
  *
  *  ===History===
  *  Originally there was only a single implementation of `ServerInterface`;
  *  the one presently named `UDPServer`.
  *
  *  Subsequent to an (OS/X) revision of the accessibility to UDP datagrams
  *  of packaged applications, it seemed to become necessary to
  *  provide at least one other that was not reliant on
  *  receiving instructions via datagrams.
  *
  *  (15 July 2022) We built a FIFO server interface, but after empirical
  *  investigation using it we concluded that packaged apps ''by default''
  *  cannot even read fifos, let alone UDP datagram ports, without the app
  *  acquiring special permissions.
  *  We have not investigated how to supply these permissions without
  *  paying Apple taxes of one or another form (not monetary).
  *  Instead we have provided a workaround that allows the packaged app
  *  to delegate to a (running) server.
  *
  */
object Server extends ServerInterface with Logging.Loggable {

  private val fifo = sys.props.get("applered.fifo") orElse sys.env.get("REDFIFO")

  if (logging && fifo.nonEmpty) fine(s"Fifo server using: ${fifo.get}")

  override def isClient: Boolean = server.isClient

  override def portName: String = server.portName

  override def startServer(): Unit =
     { server =
          if (fifo.nonEmpty) {
            warn(s"App specifies fifo service interface: but this is not feasible with this JVM. UDP service substituted.")
            UDPServer // FIFOServer // Should be
          } else
            UDPServer
       scala.swing.Swing.onEDTWait { server.startServer() }
     }

  private var server: ServerInterface = _

  override def process(arg: String): Unit = server.process(arg)

  override def stopServer(): Unit = server.stopServer()
}

