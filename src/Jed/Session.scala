package Jed

import java.io.IOException
import java.nio.file.Files


/**
 *  === Editing Session ===
 *  Construct and serveWith a GUI-managed editor session for
 *  the document with path `path` in the filestore, and associate it with
 *  a fresh numeric identity to be used as a key to the
 *  `activeReds` table.
 *
 *  If the given `location`  description
 *  is nonempty, then move the cursor to the position nearest
 *  to the one it describes. It should match the regular
 *  expression `[0-9]+([:.,][0-9]+])?`. The first group of digits is
 *  the line number; the second (if it appears) is the column number.
 *
 *  Handle session-closing, or file-opening requests
 *  from the GUI by informing the global session coordinator
 *  (`Sessions`)
 */
class Session(_path: java.nio.file.Path, val identity: Int, location: String="") {
  override def toString: String = s"Session($path, $identity)"

  val doc     = new Red.Document()
  val session = new EditSession(doc, _path.toString) with CutRing.Plugin

  // TODO: systematize the use of `Path` instead of `String` to denote filestore paths.
  def path: java.nio.file.Path = java.nio.file.Paths.get(session.path)

  try {
    val fsPath = path.normalize.toAbsolutePath
    // path = fsPath.toString
    val reader = Files.newBufferedReader(fsPath)
    doc.insert(0, reader)
  } catch {
    case exn: IOException =>
  }


  val gui     = new UI(session)

  /** Poll the gui to see if it can close */
  def close(): Unit = gui.close()

  def goTo(location: String): Unit = {
    gui.goTo(location)
    gui.makeVisible()
  }

  locally {
    gui.start()
    if (location!="") goTo(location)

    /**
     *  Declare intention to respond to a `sessionClosed` notification
     *  from the `gui` by informing the coordinator (`Sessions`)
     */
    gui.sessionClosed.handleWithTagged("CLOSER") {
      case _ =>
        Sessions.closed(this)
    }

    /** Declare intention to respond to  `openFile` requests from the GUI.
     *  This is done by locating or starting a new `Red` (at the
     *  location, if any, given).
     */
    gui.openFileRequests.handleWithTagged("OPENER") {
      case s"$fileName@$location" => Sessions.startSession(fileName, location)
      case fileName: String       => Sessions.startSession(fileName)
    }

    /**
     * Declare intention to respond to changes of path (caused by
     * invoking `SaveAs`) in the underlying `EditSession`.
     */
    session.pathChange.handleWith {
      case (from, to) => Sessions.rename(from, to)
    }

    /** Tell the coordinator that this is open. */
    Sessions.opened(this)
  }
}

