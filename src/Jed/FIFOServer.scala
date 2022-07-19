package Jed

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.Files

object FIFOServer extends Logging.Loggable with ServerInterface {
  log.level = Logging.ALL

  var processingLocally = false
  def isClient: Boolean = false

  private val fifo               = sys.props.get("applered.fifo") orElse sys.env.get("REDFIFO")

  def portName: String           = Utils.expandHome(fifo.get)
  var service:    FIFOService    = _
  var clientPort: FIFOClientPort = _


  def process(arg: String): Unit = {
    if (logging) info(s"process $arg ${if (processingLocally) " locally." else " on server."}")
    if (processingLocally)
      processLocally(arg)
    else
      clientPort.send(if (arg.startsWith("-")) arg else Utils.toAbsolutePath(arg))
  }

  def startServer(): Unit = {
    var serverAvailable = false
    // Try to contact the server

      try {
        clientPort = new FIFOClientPort(portName)
        if (logging) info(s"Acquired client port to server: $clientPort")
        serverAvailable   = true
        processingLocally = false
      } catch {
        case exn: Exception =>
          if (logging) info(s"Failed to acquire client port: $portName, because $exn")
      }

    if (!serverAvailable)
    try {
      Files.deleteIfExists(Utils.toPath(portName))
      service           = new FIFOService(portName)
      processingLocally = true
      if (logging) info(s"About to start service on $service")
      service.serveWith(processLocally)
    } catch {
      case exn: Exception =>
        if (logging) error(s"Failed to start service at $portName")
    }

  }

  def stopServer(): Unit = service.close()

  def processLocally(arg: String): Unit = {
    arg match {
      case "-probe" =>
        if (logging) fine(s"probed")
      case s"-l${module}=${level}" =>
        Logging.update(module, level)
        Logging.Default.info(s"$module=$level")
      case s"-stop" =>
        stopServer()
      case s"-quit" =>
        if (Sessions.canQuit) {
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
}

class FIFOClientPort(path: String) extends Logging.Loggable {
  val packetSize = 1024
  val serverAddress = UnixDomainSocketAddress.of(path)
  val serverChannel = SocketChannel.open(serverAddress)

  override def toString: String = s"FifoClient: $path ($serverChannel)"

  def send(message: String): Unit = {
    val sent = message+"\n"
    serverChannel.write(ByteBuffer.wrap(sent.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
  }

  def close(): Unit = {
    if (logging) fine(s"Closing client port on $serverChannel")
    serverChannel.close()
  }
}

class FIFOService(path: String) extends Logging.Loggable {
  val packetSize    = 4096
  val serverSocket  = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
  val serverAddress = UnixDomainSocketAddress.of(path)

  serverSocket.bind(serverAddress)

  override def toString: String = s"FifoService: $path"

  log.level=Logging.ALL

  def close(): Unit = {
    serverSocket.close()
    Files.deleteIfExists(serverAddress.getPath)
  }

  def serveWith(service: String => Unit): Unit = {
    var running = true
    new Thread() {
      override def run(): Unit = {
        if (logging) fine(s"server running on $serverSocket")
        while (serverSocket.isOpen) {
          val clientChannel = serverSocket.accept()
          val buf = java.nio.ByteBuffer.allocate(packetSize)
          while (clientChannel.isOpen) {
            val count = clientChannel.read(buf)
            if (count < 0) clientChannel.close() else {
              val bytes = new Array[Byte](count)
              buf.flip()
              buf.get(bytes)
              buf.flip()
              val lines = new String(bytes, java.nio.charset.StandardCharsets.UTF_8).split('\n')
              if (logging) finest(lines.mkString("Message: ", ", ", ""))
              for {message <- lines}
                javax.swing.SwingUtilities.invokeLater { case () => service(message) }
            }
          }
        }
        if (logging) fine(s"server stopped running on $serverSocket")
      }
    }.start()
  }
}
