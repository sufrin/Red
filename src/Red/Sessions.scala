package Red

import java.nio.file.Path

/**  Main program and global coordinator of editing sessions. Exits when all
  *  the editing sessions it has spawned have been closed.
  *
  *  This module keeps track of active (non-closed)
  *  `Red`s by giving each of them a fresh numeric identity, and
  *  keeping a mapping (`activeReds`) of identities to `Red`s.
  *  The identity  of a `Red` has no intrinsic
  *  meaning beyond this; but (as it happens) signifies the order in which
  *  the currently-active sessions were started.
  */
object Sessions extends Logging.Loggable {
  // Log warnings and errors

  /**  Start an editing session for each of the paths named in `args`
    *  If a document exists at that path, then the session edits the document
    *  already stored there.
    *
    *  If no such document exists, the session edits a fresh, blank,
    *  document that is destined to be stored at that path in the filestore.
    *
    *  If an argument is of the form `-l`''module''`=`''level'' then the logging
    *  level for the named module is set to the (named) level.
    */

  def main(args_ : Array[String]): Unit = {
    val args = args_.toList
    if (args.isEmpty) {
      exitOnLastClose = false
      Server.process(Utils.freshDocumentName())
    } else
      for { arg <- args } Server.process(arg)
    if (logging) finer(s"$Server finished")
  }

  /** Yields a new `Red`, editing the document in
    * (or destined to be saved in) the filestore at `fileName`.
    */
  def startSession(fileName: String): Session = {
    if (logging) info(s"startSession($fileName)")
    findRed(fileName) match {
      case None      => new Session(Utils.toPath(fileName), newRedIndex())
      case Some(red) => red
    }
  }

  def startSession(fileName: String, location: String): Session = {
    if (logging) info(s"startSession($fileName, $location)")
    val red = startSession(fileName)
    red.goTo(location)
    red
  }

  private var _redIndex: Int = 0
  private def newRedIndex(): Int = { _redIndex += 1; _redIndex }

  /** Mapping from session identities to active `Red`s */
  val activeReds: collection.mutable.HashMap[Int, Session] =
    new collection.mutable.HashMap[Int, Session]

  def forActiveReds(op: Session => Unit): Unit =
    for { (_, red) <- activeReds } op(red)

  def closed(red: Session): Unit = {
    activeReds -= red.identity
    info(s"Finished editing ${red.path} at ${Utils.dateString()}")
    forActiveReds { red =>
      info(s"Still editing ${red.path} (#${red.identity})")
    }
    if (activeReds.isEmpty && Sessions.exitOnLastClose) System.exit(1)
  }

  var exitOnLastClose: Boolean = true

  def opened(red: Session): Unit = {
    activeReds += red.identity -> red
    Utils.Recents.add(red.path.toAbsolutePath.toString)
  }

  /** Find an active `Red` for a file with the same name, if any. */
  def findRed(fileName: String): Option[Session] = {
    val path = Utils.toPath(fileName)
    activeReds.collectFirst {
      case (_, red) if red.path == path => red
    }
  }

  /** About to rename the session for `oldPath` to a session for `newPath`, because
    * its document has changed path, having been subject to a "Save as...".
    * The corresponding `EditSession`'s path itself has not yet been renamed:
    * it will still be `fromPath`.
    */
  def rename(fromPath: Path, toPath: Path): Unit = {
    if (logging) warn(s"Renaming session $fromPath to $toPath")
    if (logging) forActiveReds { red =>
      info(s"Before rename ${red.path} (#${red.identity})")
    }
    activeReds.collectFirst {
      case (_, red) if red.path == fromPath => red
    } match {
      case None =>
        warn(s"No session at path: $fromPath")
      case Some(red) =>
        if (logging)
          info(s"Renaming session $fromPath (#${red.identity}) to $toPath")
        activeReds -= red.identity
        activeReds += red.identity -> red
        if (logging) forActiveReds { red =>
          info(s"After rename ${red.path} (#${red.identity})")
        }
    }
  }

  /** Is the ''entire application'' in a state where
    *  quitting without data loss is possible.
    *
    *  First approximation: only if there are no
    *  activeReds sessions that the user is still
    *  interested in.
    */
  def canQuit: Boolean = {
    forActiveReds(_.close())
    activeReds.isEmpty
  }

}
