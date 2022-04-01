package Red

import java.awt.{Color, Component, Font, Graphics}
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.Icon
import scala.swing.Image

/**
 * System wide default settings. These will eventually be treated as (dynamic)
 * preferences.
 */
object Settings {
  var defaultFont:   Font      = new Font("Monospaced", Font.PLAIN,  18)
  var buttonFont:    Font      = new Font("Monospaced", Font.PLAIN,  18)
  var widgetFont:    Font      = new Font("Monospaced", Font.ITALIC, 18)
  var feedbackFont:  Font      = new Font("Monospaced", Font.PLAIN,  16)
  var feedbackColor: Color     = Color.BLUE

  /** True when clicking at a bra/ket should select
   *  the bracketed material. Controlled from the GUI. */
  var clickSelects: Boolean = true

  var dateFormat: SimpleDateFormat = new SimpleDateFormat("y-MM-dd@HHmmss")

  def dateString(time: Long): String = dateFormat.format(new Date(time))
  def dateString(): String           = dateFormat.format(new Date())

  private class ColoredIcon(h: Int, w: Int, color: java.awt.Color) extends Icon {
    def getIconHeight: Int = h
    def getIconWidth: Int  = w
    override def paintIcon(c: Component, g: Graphics, x: Int, y: Int): Unit = {
      g.setColor(color)
      g.fill3DRect(0, 0, w, h, true)
    }
  }

  val closeIcon: Icon  = new ColoredIcon(60, 60, Color.GREEN)
  val redIcon:   Icon  = new ColoredIcon(60, 60, Color.RED)

  private val fileSeparator: String = System.getProperty("file.separator")

  // TODO: do these manipulations with java.nio.Path
  /** Transform a file path to the path suitable for saving a
   *  primary backup file (a copy of the original file at
   *  `filePath`).
   */
  def filePath2Backup(filePath: String): String          = s"${filePath}~"

  /** Transform a directory path to the path of a directory suitable
   *  for saving secondary backups (copies of the original primary
   *  backup, made when that is about to change).
   */
  def filePath2SecondaryFolder(filePath: String): String = s"${filePath}~-~"

  /** Transform a file path to the path where a secondary backup for
   *  that file should be stored, if its primary backup is about to
   *  change.
   */
  def filePath2SecondaryBackup(filePath: String): String = {
      import java.io.File
      val folder    = filePath2SecondaryFolder(filePath)
      val name      = new File(filePath).getName
      s"${folder}${fileSeparator}${name}(${dateString()})~"
  }
}

