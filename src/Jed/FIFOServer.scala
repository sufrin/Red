package Jed

object FIFOServer extends Logging.Loggable with ServerInterface {
  log.level = Logging.ALL

  def isClient: Boolean = false

  val fifo    = sys.props.get("applered.fifo") orElse sys.env.get("REDFIFO")
  def portName = fifo.get
  val port = new FIFOPort(portName)

  def process(arg: String): Unit = processLocally(arg)

  def startServer(): Unit = {
    Utils.invokeLater { Red.AppleRed.establishMainWindowFrame(portName) }
    port.start(processLocally(_))
  }

  def stopServer(): Unit = port.close()

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
        if (Sessions.canQuit()) {
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
  }
}

class FIFOPort(path: String) extends Logging.Loggable {
  val fifo = new java.io.FileInputStream(path)

  override def toString: String = s"Fifo: $path"

  log.level=Logging.ALL

  def readLine(): String = {
    val s = new StringBuilder()
    var reading = true
    try {
      if (logging) fine("readLine()")
      while (reading) {
        fifo.read() match {
          case -1 => reading = false
          case '\n' =>
          case ch => s.append(ch.toChar)
        }
      }
    } catch {
      case exn: java.io.IOException =>
        exn.printStackTrace()
        reading=false
    }
    s.toString()
  }

  def close(): Unit = fifo.close()

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
