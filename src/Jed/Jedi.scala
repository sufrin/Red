package Jed

import Red.Document

import java.io.IOException
import java.nio.file.Files

/** Start a UI on a session managing the document
 *  in the filestore at `path`, if it exists; and
 *  an empty document if it doesn't exist.
 */
class Jedi (var path: String) {
  val doc     = new Document()
  try {
    val fsPath = java.nio.file.Paths.get(path).normalize.toAbsolutePath
    path = fsPath.toString
    val reader = Files.newBufferedReader(fsPath)
    doc.insert(0, reader)
  } catch {
    case exn: IOException => path = s"$path${Utils.NEWFILESUFFIX}"
  }

  val session = new EditSession(doc, path) with CutRing.Plugin
  val ui      = new UI(session)

  locally { ui.start() }
}

object Jedi {
  def main(args: Array[String]): Unit = {
    for  { arg <- args } { process(arg) }
  }

  def process(arg: String): Unit =
    arg match {
      case s"-l${module}=${level}" =>
        Logging.update(module, level)
        Logging.Default.info(s"$module=$level")
      case s"-l${module}" =>
        Logging.update(module, "ALL")
        Logging.Default.info(s"$module=ALL")
      case path =>
        new Jedi(path)
    }
}
