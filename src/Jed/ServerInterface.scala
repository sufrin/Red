package Jed

trait ServerInterface {
  def startServer(): Unit
  def process(arg: String): Unit
  def stopServer(): Unit
  def portName: String
  def isClient: Boolean             // non-server program: delegates requests
  def isServer: Boolean = !isClient // server program: delgate for requests
  // True for an OS/X packaged app
  def isApp: Boolean = sys.props.get("applered.app").nonEmpty
}
