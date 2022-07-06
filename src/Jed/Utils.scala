package Jed

import Red.DocumentInterface

import java.awt.{Color, Component, Font, Graphics}
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.Icon

/** System wide default settings. These will eventually be treated as (dynamic)
  * preferences.
  */
object Utils {
  var defaultFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var buttonFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var widgetFont: Font = new Font("Monospaced", Font.ITALIC, 18)
  var feedbackFont: Font = new Font("Monospaced", Font.PLAIN, 16)
  var feedbackColor: Color = Color.BLUE

  private val dateFormat: SimpleDateFormat = new SimpleDateFormat("y-MM-dd@HHmmss")

  def dateString(time: Long): String = dateFormat.format(new Date(time))
  def dateString(): String = dateFormat.format(new Date())

  class ColoredIcon(h: Int, w: Int, color: java.awt.Color)
    extends Icon {
    def getIconHeight: Int = h
    def getIconWidth: Int = w
    override def paintIcon(c: Component, g: Graphics, x: Int, y: Int): Unit = {
      g.setColor(color)
      g.fill3DRect(0, 0, w, h, true)
    }
  }

  val closeIcon: Icon = new ColoredIcon(60, 60, Color.GREEN)
  val redIcon: Icon = new ColoredIcon(60, 60, Color.RED)

  private val fileSeparator: String = System.getProperty("file.separator")

  /** Transform a file path to the path suitable for saving a
    *  primary backup file (a copy of the original file at
    *  `filePath`).
    */
  def filePath2Backup(filePath: String): String = s"${filePath}~"

  /** The suffix added to the name of a file when there is no correspondingly named file
   * in the filestore as an editor session starts.
   */
  val NEWFILESUFFIX: String = "«NEW»"

  /** Save the given `document` in the filestore at the specified path.
    *
    * @param path specification of a filestore path at which to save the document. This
    *             is either this parameter itself, or the parameter stripped of the `NEWFILESUFFIX`.
    * @param document the document to be saved.
    * @return the path at which the document was actually saved.
    *
    * NB: the use of the "«NEW»" suffix is a detail that would be replaced in a
    * production-quality program.
    */
  def save(path: String, document: DocumentInterface): String = {
    val thePath = if (path.endsWith(NEWFILESUFFIX))
                     path.substring(0, path.length-NEWFILESUFFIX.length)
                  else path
    backup(path)
    val writer = java.nio.file.Files.newBufferedWriter(
      new java.io.File(thePath).toPath,
      java.nio.charset.Charset.forName("UTF-8")
    )
    document.writeTo(writer)
    writer.close()
    thePath
  }

  import java.nio.file.{Path,Paths,Files}

  def save(path: Path, document: DocumentInterface): String = save(path.toString, document)

  /** If a file exists in the filestore at `path`, then copy it to a new file at a path
   *  derived from `path` by adding a string derived from the time at which the existing
   *  file was written, and ending in a `"~"`.
   *
   *  This is crude, but safer than trying to manage without backups at all,
   *  or with just a single backup. Modern filestores are large enough to cope
   *  with storing a sequence of backups; and they obviously need not be kept
   *  indefinitely.
   *
   *  By invoking this method just before saving a document to the filestore,
   *  the very latest saved copy of an edited document will be found at the right place
   *  along with a (perhaps empty) sequence of its predecessors -- all with the
   *  same prefix, and with suffixes that permit easy and systematic tidying up.
   */
  def backup(path: String): Unit = {
    import java.nio.file.{Files, StandardCopyOption}
    val thePath = new File(path).toPath
    if (Files.exists(thePath)) {
      val fileTime = try  Files.getLastModifiedTime(thePath).toMillis catch {
        case _: Exception => 0L
      }
      val backupPath = new File(s"$path-${dateString(fileTime)}~").toPath
      Files.copy(thePath, backupPath, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  def toPath(path: String): Path = Paths.get(expandHome(path)).toAbsolutePath()

  def toParentPath(path: String): Path = toPath(path).getParent.toAbsolutePath()

  def expandHome(path: String): String =
    if (path.startsWith("~")) path.replaceFirst("^~", System.getProperty("user.home")) else path

  def checkWriteable(path: String): Option[String] = {
    val theParent = toParentPath(path)
    val thePath   = toPath(path)
    if (Files.exists(thePath) && !Files.isWritable(thePath))  Some(s"Exists, but not writable: $thePath") else
    if (!Files.exists(theParent) || !Files.isDirectory(theParent)) Some(s"Not a folder: $theParent") else
    if (Files.exists(theParent) && !Files.isWritable(theParent)) Some(s"Folder exists, but not writable: $theParent")
    else None
  }

  lazy val homePath: Path = Paths.get(System.getProperty("user.home"))

  /**
   *  `thePath` as a string relative to the user's home directory, if possible.
   *  This leads to shorter feedback messages, without information loss.
   */
  def relativeToHome(thePath: Path): String = {
    if (thePath.isAbsolute && thePath.startsWith(homePath)) {
      val rel = thePath.subpath(homePath.getNameCount, thePath.getNameCount)
      s"~${System.getProperty("file.separator")}${rel.toString}"
    }
    else
      thePath.toString
  }

  def relativeToHome(thePath: String): String = relativeToHome(toPath(thePath))

  def relativeToGrandparent(thePath: String): String = {
    val path = Paths.get(thePath)
    val count = path.getNameCount
    if (count>2) path.subpath(count-2, count).toString else path.toString
  }

  def displayablePath(thePath: String): String = {
    val homeRel = relativeToHome(thePath)
    val prefix  = if (homeRel.startsWith("~")) "~/..." else "..."
    if (homeRel.length>60) s"$prefix/${relativeToGrandparent(thePath)}" else homeRel
  }

}
