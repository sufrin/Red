package Red

/**  Interface to a Red server or its implementation
  */
trait ServerInterface {
  def startServer(): Unit
  def process(arg: String): Unit
  def stopServer(): Unit
  def portName: String
  def isClient: Boolean // non-server program: delegates requests
  def isServer: Boolean = !isClient // server program: a delegate for requests

  // True for an OS/X packaged app while we are debugging
  def isOSXApp: Boolean = sys.props.get("applered.app").nonEmpty
}
