package Red

import java.awt.{Color, Component, Font, Graphics}
import java.io.File
import java.nio.file.attribute.FileTime
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.{Icon, SwingUtilities}
import scala.collection.mutable.ListBuffer
import scala.swing.{Action, Alignment, Button, CheckMenuItem, Image, MenuItem, event}
import scala.sys.process.Process

/** System wide default settings. These will eventually be treated as (dynamic)
  * preferences.
  */
object Utils {
  var defaultFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var smallButtonFont: Font = new Font("Monospaced", Font.BOLD, 24)
  var buttonFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var menuFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var menuButtonFont: Font = new Font("Monospaced", Font.PLAIN, 18)
  var widgetFont: Font = new Font("Monospaced", Font.BOLD, 16)
  var feedbackFont: Font = new Font("Monospaced", Font.PLAIN, 16)
  var feedbackColor: Color = Color.BLUE

  def setFont(role: String, kind: String, weight: String, size: String): Unit = {
    val k = "Monospaced"
    val w = weight match {
      case "plain"  => Font.PLAIN
      case "bold"   => Font.BOLD
      case "italic" => Font.ITALIC
      case _ => Font.PLAIN
    }
    val s = if (size.matches("[0-9]+")) size.toInt else 16
    val f = new Font(k, w, s)
    role match {
      case "default"      => defaultFont = f
      case "button"       => buttonFont = f
      case "button-small" => smallButtonFont = f
      case "small-button" => smallButtonFont = f
      case "menu"         => menuFont = f
      case "button-menu"  => menuButtonFont = f
      case "menu-button"  => menuButtonFont = f
      case "widget"       => widgetFont = f
      case "feedback"     => feedbackFont = f
      case _              => defaultFont = f
    }
  }

  /** Miscellaneous utilities for manipulating images and icons */
  object ImageUtilities {

    /** Make an Image from an Icon */
    def makeImage(icon: Icon): Image = {
      import java.awt.GraphicsEnvironment
      val w = icon.getIconWidth
      val h = icon.getIconHeight
      val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
      val gd = ge.getDefaultScreenDevice
      val gc = gd.getDefaultConfiguration
      val image = gc.createCompatibleImage(w, h)
      val g = image.createGraphics
      icon.paintIcon(null, g, 0, 0)
      g.dispose()
      image
    }

    class ColoredIcon(h: Int, w: Int, color: java.awt.Color) extends Icon {
      def getIconHeight: Int = h

      def getIconWidth: Int = w

      override def paintIcon(
          c: Component,
          g: Graphics,
          x: Int,
          y: Int
      ): Unit = {
        g.setColor(color)
        g.fill3DRect(0, 0, w, h, true)
      }

    }

  }
  val redIcon: Icon = new ImageUtilities.ColoredIcon(60, 60, Color.RED)
  val redImage: Image = ImageUtilities.makeImage(redIcon)

  private val dateFormat: SimpleDateFormat = new SimpleDateFormat(
    "y-MM-dd-HHmmss"
  )
  def dateString(time: Long): String = dateFormat.format(new Date(time))
  def dateString(): String = dateFormat.format(new Date())

  class Menu(title: String) extends scala.swing.Menu(title) {
    font = menuFont
  }

  /** A mixed-mode dynamic+static menu presented in order: prefix ++ dynamic ++ suffix */
  abstract class DynamicMenu(title: String) extends Menus.DynamicMenu(title) {
    font = menuFont
    val prefix, suffix: collection.mutable.Buffer[scala.swing.Component] =
      new ListBuffer[scala.swing.Component]
    def content: Seq[scala.swing.Component] =
      prefix.toList ++ dynamic ++ suffix.toList
    def dynamic: Seq[scala.swing.Component]
  }

  abstract class LazyDynamicMenu(title: String, titles: => Seq[String])
      extends Menus.LazyDynamicMenu(title, titles) {
    font = menuFont
    val prefix, suffix: collection.mutable.Buffer[scala.swing.Component] =
      new ListBuffer[scala.swing.Component]
    override def content: Seq[scala.swing.Component] =
      prefix.toList ++ super.content ++ suffix.toList
  }

  /** A ''completely'' dynamic menu that evaluates  `_dynamic` to generate content on popup */
  object DynamicMenu {
    def apply(title: String)(
        _dynamic: => Seq[scala.swing.Component]
    ): DynamicMenu = new DynamicMenu(title) {
      def dynamic: Seq[scala.swing.Component] = _dynamic
    }
  }

  def Item(name: String, toolTip: String = "", itemFont: Font = Utils.menuButtonFont)(act: => Unit): MenuItem =
    new MenuItem(Action(name) {
      act
    }) {
      font = itemFont
      if (toolTip.nonEmpty) tooltip = toolTip
    }

  def Button(name: String, toolTip: String = "")(act: => Unit): swing.Button =
    new Button(Action(name) {
      act
    }) {
      font = Utils.buttonFont
      if (toolTip.nonEmpty) tooltip = toolTip
      horizontalAlignment = Alignment.Center
      verticalAlignment = Alignment.Center
    }

  //private val fileSeparator: String = System.getProperty("file.separator")

  /** Transform a file path to the path suitable for saving a
    *  primary backup file (a copy of the original file at
    *  `filePath`).
    */
  def filePath2Backup(filePath: String): String = s"$filePath~"

  /** The suffix added to the name of a file when there is no correspondingly named file
    * in the filestore as an editor session starts.
    */
  val NEWFILESUFFIX: String = "«NEW»"

  /** Save the given `document` in the filestore at the specified path.
    *
    * @param path specification of a filestore path at which to save the document.
    * @param document the document to be saved.
    * @return None if the save was successful; Some(reasonMessage) if it was not successful
    */
  def save(path: String, document: DocumentInterface): Option[String] =
    checkWriteable(path) match {
      case None =>
        try {
          backup(path)
          val writer = java.nio.file.Files.newBufferedWriter(
            new java.io.File(path).toPath,
            java.nio.charset.Charset.forName("UTF-8")
          )
          document.writeTo(writer)
          writer.close()
          None
        } catch {
          case exn: Exception => Some(exn.getMessage)
        }

      case reason => reason
    }

  import java.nio.file.{Files, Path, Paths}

  def save(path: Path, document: DocumentInterface): Option[String] =
    save(path.toString, document)

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
    *
    *  ===Backup Detail
    *
    *      A file with path ending with ''dirname''`/`''filename'' will have backup(s)
    *      in the same directory named:
    *
    *      ''filename''`+`''timestamp''`~`
    *
    *      and (as a convenience) the latest of these will also have a link to it
    *      in the same directory, named:
    *
    *      ''filename''`~`
    *
    *      and the earliest backup, if there is more than one, will have a link to it
    *      in the same directory, named:
    *
    *      ''filename''`~~`
    *
    *      On a Unix machine, the timestamped backup copies of this file can be
    *      removed with the command: `rm `''dirname''`/`''filename''`+*~`
    */
  def backup(path: String): Unit = {
    import java.nio.file.{Files, StandardCopyOption}
    var thePath = new File(path).toPath
    var fileTime: FileTime = null
    if (Files.exists(thePath)) {
      try { fileTime = Files.getLastModifiedTime(thePath) }
      finally {}
      val backupPath = new File(
        s"$path+${dateString(fileTime.toMillis)}~"
      ).toPath
      Files.copy(thePath, backupPath, StandardCopyOption.REPLACE_EXISTING)
      Files.setLastModifiedTime(backupPath, fileTime)
      // create a convenience link
      val earliestBackupLink = new File(s"$path~~").toPath
      val latestBackupLink = new File(s"$path~").toPath
      if (Files.exists(latestBackupLink) && !Files.exists(earliestBackupLink)) {
        Files.createLink(earliestBackupLink, latestBackupLink)
      }
      Files.deleteIfExists(latestBackupLink)
      Files.createLink(latestBackupLink, backupPath)

    }
  }

  def localizePath(thePath: String, cwd: Path, plusPath: Path): String = {
    thePath match {
      case s"~/$_" => expandHome(thePath)
      case s"+$_"  => plusPath.resolve(thePath).toString
      case s"+/$_" => plusPath.resolve(thePath).toString
      case s"/$_"  => thePath
      case path    => cwd.resolve(path).toString
    }
  }

  def freshDocumentName(): String = s"New=${dateString()}"

  /**  Translate filename arguments to absolute paths
    *  relative to to the current working directory.
    */
  def toAbsolutePath(arg: String): String =
    if (arg.startsWith("-")) arg
    else {
      import java.nio.file.Path
      Path.of(arg).toAbsolutePath.toString
    }

  def toPath(path: String): Path = Paths.get(expandHome(path)).toAbsolutePath

  def toParentPath(path: String): Path = toPath(path).getParent.toAbsolutePath

  def expandHome(path: String): String =
    if (path.startsWith("~"))
      path.replaceFirst("^~", System.getProperty("user.home"))
    else path

  /** Return `Some(reasonMessage)` if the given `path` does not denote a writable
    * file in the filestore. Otherwise return `None`.
    */
  def checkWriteable(path: String): Option[String] = {
    val theParent = toParentPath(path)
    val thePath = toPath(path)
    if (Files.exists(thePath) && Files.isDirectory(thePath))
      Some(s"This path is a directory/folder: $thePath")
    else if (Files.exists(thePath) && !Files.isWritable(thePath))
      Some(s"Exists, but not writable: $thePath")
    else if (!Files.exists(theParent) || !Files.isDirectory(theParent))
      Some(s"Not a directory/folder: $theParent")
    else if (Files.exists(theParent) && !Files.isWritable(theParent))
      Some(s"Directory/folder exists, but not writable: $theParent")
    else None
  }

  lazy val homePath: Path = Paths.get(System.getProperty("user.home"))

  /**  `thePath` as a string relative to the user's home directory, if possible.
    *  This leads to shorter feedback messages, without information loss.
    */
  def relativeToHome(thePath: Path): String = {
    if (thePath.isAbsolute && thePath.startsWith(homePath)) {
      val rel = homePath.relativize(
        thePath
      ) //.subpath(homePath.getNameCount, thePath.getNameCount)
      s"~${System.getProperty("file.separator")}${rel.toString}"
    } else
      thePath.toString
  }

  def relativeToHome(thePath: String): String = relativeToHome(toPath(thePath))

  /** Construct an abbreviated path suitable for displaying in a narrow space */
  def relativeToGrandparent(thePath: String): String = {
    val path = Paths.get(thePath)
    val count = path.getNameCount
    if (count > 2) path.subpath(count - 2, count).toString else path.toString
  }

  def displayablePath(thePath: String): String = {
    val homeRel = relativeToHome(thePath)
    val prefix = if (homeRel.startsWith("~")) "~/..." else "..."
    if (homeRel.length > 60) s"$prefix/${relativeToGrandparent(thePath)}"
    else homeRel
  }

  def invokeLater(act: => Unit): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = act
    })
  }

  def invokeAndWait(act: => Unit): Unit = {
    SwingUtilities.invokeAndWait(new Runnable {
      override def run(): Unit = act
    })
  }

  /**    A non-EDT worker that runs `doOffEDT()` off the event dispatch thread
    *    and passes buffered `publish`ed `Report`s to `report`.
    */
  abstract class OffEdtThread[Result, Report](
      report: Report => Unit,
      finished: => Unit
  ) extends javax.swing.SwingWorker[Result, Report] {

    def doOffEDT(): Result

    override def doInBackground(): Result = doOffEDT()

    protected override def done(): Unit = finished

    /** Pass buffered `Report`s one by one to `report`. */
    protected override def process(buffer: java.util.List[Report]): Unit = {
      val it = buffer.iterator()
      while (it.hasNext) { report(it.next()) }
    }
  }

  /** Return the PID of the current process if possible
    */
  def getPID: String = {
    try {
      val stat = io.Source.fromFile("/proc/self/stat")
      val s = new StringBuilder("PID: ")
      var reading = true
      while (reading) {
        stat.next() match {
          case ch if '0' <= ch && ch <= '9' =>
            s.append(ch)
          case _ =>
            reading = false
        }
      }
      s.toString()
    } catch {
      case _: Exception => ""
    }
  }

  /** serveWith a new server and run it in the background */
  def startRedServerProcess(portName: String): Unit = {
    val stdin = new java.io.ByteArrayInputStream("".getBytes)
    val offEDT: OffEdtThread[Unit, Unit] =
      new OffEdtThread[Unit, Unit]({ _ => () }, { () }) {
        val cmd = List("appleredserver", portName)
        def doOffEDT(): Unit = (Process(cmd) #< stdin).!
      }
    offEDT.execute()
  }

  lazy val preferences = java.util.prefs.Preferences.userRoot()
  lazy val appleRed    = preferences.node("/AppleRed")
  lazy val appleRedUI  = appleRed.node("/UI")

  object Recents {
    lazy val paths       = collection.mutable.ListBuffer[String]()
    lazy val recents     = appleRed.node("/recents")
    lazy val count       = recents.getInt("count", 0)
    locally { for { i <- 0 until count } paths.addOne(recents.get(s"$i", "")) }
    def add(path: String): Unit = {
      if (!paths.contains(path)) {
        while (paths.length >= 5) paths.remove(0)
        paths.addOne(path)
        sync()
      }
    }
    def get: Seq[String] = paths.toList ++ List("-")
    def forget(): Unit = {
      paths.clear()
      sync()
    }
    def sync(): Unit = {
      recents.putInt("count", paths.length)
      for {i <- 0 until paths.length} recents.put(s"$i", paths(i))
      recents.sync()
    }
  }

  /**
   *  A persistent `CheckMenuItem`
   */
  class CheckItem(name: String) extends CheckMenuItem(name) {
    selected = appleRedUI.getBoolean(name, false)
    reactions += {
      case event.ButtonClicked(_) =>
        appleRedUI.putBoolean(name, selected)
        appleRedUI.sync()
    }
  }
}
