package Red

/**  Simple one-way interprocess communication on the local host by acknowledged (short)
 *  datagrams. Intended for low-bandwidth, low-contention use.
 *
 *  '''NB'''
 *  The implementation actually uses internet domain sockets on the local
 *  host. It would be (a little) more secure if it used Unix domain
 *  sockets; but these are not implemented in some variants of Java.
 */
object Bus {

  import java.net.{DatagramPacket, DatagramSocket, InetAddress}

  val LOCALHOST: InetAddress = InetAddress.getByName("localhost")
  private
  val UTF8:      String      = java.nio.charset.StandardCharsets.UTF_8.name

  /** Representation of message that has been sent ''from'' `port` */
  case class Datagram(port: Int, message: String)

  /** A source of `Datagram`, listening at `port`, with the specified buffer size. */
  class Receiver(port: Int, bufferSize: Int = 4096) {
    val socket: DatagramSocket =
      try { new DatagramSocket(port) }
      catch {
        case exn: Exception =>
          exn.printStackTrace()
          null
      }
    socket.setReceiveBufferSize(bufferSize)
    val buffer = new Array[Byte](bufferSize)
    val packet = new DatagramPacket(buffer, bufferSize)
    private var running: Boolean = false

    def stop(): Unit = {
      running = false
      socket.close()
    }

    def isClosed: Boolean = socket.isClosed

    /** Repeatedly read datagrams from the socket, and invoke `service`
      * on its `message`. Terminates when the socket has been closed by
      * `stop()`.
      */
    def start(service: String => Unit): Unit = {
      running = true
      new Thread() {
        override def run(): Unit = {
          while (running) {
            receive() match {
              case Some(Datagram(_, message)) =>
                javax.swing.SwingUtilities.invokeLater { case () =>
                  service(message)
                }
              case None =>
                running = false
            }
          }
        }
      }.start()
    }

    /** Return `Some(d)` where `d` is the next `Datagram`
      * read from the socket; or `None` if the socket was
      * closed during the read.
      */
    def receive(): Option[Datagram] = {
      try {
        socket.receive(packet)
        val result = new Array[Byte](packet.getLength)
        System.arraycopy(buffer, 0, result, 0, result.length)
        // ACK
        val sendPort = packet.getPort
        val ack = s"ACK $sendPort".getBytes(UTF8)
        val acket = new DatagramPacket(ack, 0, ack.length, LOCALHOST, sendPort)
        try { socket.send(acket) }
        finally ()
        //
        Some(Datagram(sendPort, new String(result, UTF8)))
      } catch {
        case exn: Exception =>
          exn.printStackTrace()
          None
      }
    }

    override def toString: String =
      s"IPC.Receiver(localhost:${socket.getLocalPort} [$bufferSize])"
  }

  /** A sender for messages, for which acknowledgements must be received
    * by `ackTimeMS` milliseconds after they were sent.
    */
  class Sender(ackTimeMS: Int = 2000) {
    val socket = new DatagramSocket()
    socket.setSoTimeout(ackTimeMS)

    /** Send `message` in a `Datagram` to `port`, and return
      * `Some(ack)`  if it was acknowledged with `ack` before the timeout,
      * and `None` if it timed out or if the socket was closed.
      */
    def send(port: Int, message: String): Option[Datagram] = {
      val bytes = message.getBytes(UTF8)
      val packet = new DatagramPacket(bytes, 0, bytes.length, LOCALHOST, port)
      socket.send(packet)
      // Expected ACK
      val sender = socket.getPort
      val ack = s"ACK $sender".getBytes(UTF8)
      val buffer = new Array[Byte](ack.length)
      val acket = new DatagramPacket(buffer, buffer.length)
      try {
        socket.receive(acket)
        Some(Datagram(acket.getPort, new String(buffer, UTF8)))
      } catch {
        case _: Exception => None
      }
    }

    override def toString: String =
      s"IPC.Sender(localhost:${socket.getLocalPort})"
  }

}
