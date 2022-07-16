package Jed

trait ServerInterface {
  def startServer(): Unit
  def process(arg: String): Unit
  def stopServer(): Unit
  def portName: String
  def isClient: Boolean // non-server program that delegates requests
  // True for an OS/X packaged app that delegates requests
  def isApp: Boolean = sys.props.get("applered.app").nonEmpty
}
