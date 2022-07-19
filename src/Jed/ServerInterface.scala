package Jed

trait ServerInterface {
  def startServer(): Unit         = throw new UnsupportedOperationException
  def process(arg: String): Unit  = throw new UnsupportedOperationException
  def stopServer(): Unit          = throw new UnsupportedOperationException
  def portName: String            = throw new UnsupportedOperationException
  def isClient: Boolean           = throw new UnsupportedOperationException         // non-server program: delegates requests
  def isServer: Boolean           = !isClient // server program: delgate for requests
  // True for an OS/X packaged app
  def isApp: Boolean              = sys.props.get("applered.app").nonEmpty
}
