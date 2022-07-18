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
  def portName: String           = fifo.get
  var serverPort: FIFOServerPort = _
  var clientPort: FIFOClientPort = _


  def process(arg: String): Unit = {
    if (logging) info(s"process $arg ${if (processingLocally) " locally." else " on server."}")
    if (processingLocally)
      processLocally(arg)
    else
      clientPort.send(if (arg.startsWith("-")) arg else Utils.toAbsolutePath(arg))
  }

  def startServer(): Unit = {
    // Try to contact the server
    try {
      serverPort = new FIFOServerPort(portName)
      processingLocally = true
      serverPort.start(processLocally)
    } catch {
      case exn: Exception =>
        processingLocally = false
      // exn.printStackTrace()
    }
    if (!processingLocally) {
      try {
        clientPort = new FIFOClientPort(portName)
      } catch {
        case exn: Exception =>
          processingLocally = true
          exn.printStackTrace()
      }
    }
  }

  def stopServer(): Unit = serverPort.close()

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

  def send(message: String): Unit = {
    serverChannel.write(ByteBuffer.wrap(message.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
  }

  def close(): Unit = {
    serverChannel.close()
  }
}

class FIFOServerPort(path: String) extends Logging.Loggable {
  val packetSize    = 1024
  val serverSocket  = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
  val serverAddress = UnixDomainSocketAddress.of(path)
  serverSocket.bind(serverAddress)

  override def toString: String = s"Fifo: $path"

  log.level=Logging.ALL

  def readLine(): String = {
    try {
      val clientChannel = serverSocket.accept()
      val buf = java.nio.ByteBuffer.allocate(packetSize)
      val count = clientChannel.read(buf)
      val bytes = new Array[Byte](count)
      buf.flip()
      buf.get(bytes)
      new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    } catch {
      case exn: java.io.IOException =>
        exn.printStackTrace()
        ""
    }
  }

  def close(): Unit = {
    serverSocket.close()
    Files.deleteIfExists(serverAddress.getPath)
  }

  def start(service: String => Unit): Unit = {
    var running = true
    new Thread() {
      override def run(): Unit = {
        println("server running")
        while (running) {
          readLine() match {
            case ""      =>
              running = false
            case message =>
              javax.swing.SwingUtilities.invokeLater { case () => service(message) }
          }
        }
      }
    }.start()
  }
}
