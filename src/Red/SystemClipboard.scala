package Red

import java.awt.Toolkit
import java.awt.datatransfer._
import java.io.BufferedReader

/** `SystemClipboard` provides access to the system Clipboard. We cut characters
  * to it, and paste characters from it.
  *
  * TODO: implement a `DataFlavor` that can represent a `Cut` (ie, a text
  * with its "extent")
  */
object SystemClipboard extends Logging.Loggable {
  val sysClip: Clipboard      = Toolkit.getDefaultToolkit.getSystemClipboard
  val unicodeText: DataFlavor = DataFlavor.stringFlavor

  /** Needed by the API but apparently not used */
  private val owner = new ClipboardOwner() {
    def lostOwnership(sysClipBoard: Clipboard, tx: Transferable): Unit = {
      fine(s"lostOwnership(%$sysClipBoard, $tx)")
    }
  }

  def set(text: String): Unit = {
    fine(s"set(\"$text\")")
    sysClip.setContents(new StringSelection(text), owner)
  }

  def getOrElse(default: String): String = get.getOrElse(default)

  def get: Option[String] = {
    val tx = sysClip.getContents(owner)
    if (tx != null && tx.isDataFlavorSupported(unicodeText)) try {
      /* Our own component will read an empty last line properly
         (Unlike the stock BufferedReader/LineReader components)
       */
      val res  = new TextBuffer()
      val clip = new BufferedReader(unicodeText.getReaderForText(tx))
      res.insert(0, clip)
      Some(res.toString)
    } catch { case _: Exception => None }
    else None
  }
}
