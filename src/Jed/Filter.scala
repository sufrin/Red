package Jed

import Commands.StateChange
import FilterUtilities.inputStreamOf
import Jed.EditSessionCommands.{SessionCommand, StateChangeOption}
import Red.Notifier

import scala.sys.process.{ProcessBuilder, ProcessLogger}

/** A filter is a component that transforms the
*  currently-selected text, copies the selection, then
*  swaps the transformed text with the current selection.
*
*  Filters may be "intrinsic" or "external". The latter
*  pipe the text through an external program, or in
*  exceptional cases run external programs without
*  passing the selected text to them.
*
*/

abstract class Filter extends SessionCommand {
  thisTransform =>

  /** Running a filter applies `transform` to the input to be transformed
   * (the current selection), then if the transform "succeeds", it swaps
   * the result with the current selection.
   *
   * Exceptions arising during the transformation are
   * notified (as strings) via the `Filter.warnings` handler.
   *
   * If `adjustNL` is true (the default) and the input ends in a
   * newline, then the transform is applied to all but that character.
   *
   * The `kind` of the filter (default `"Nothing"`) is inherited by the `StateChange` of
   * a successful transformation.
   *
   */
  protected def transform(input: String): Option[String] = None

  protected def adjustNL: Boolean = true

  protected val kind: String = "Nothing"

  protected def handle(exn: Exception): Unit =
    Filter.warnings.notify("Transform", exn.toString)

  def DO(session: EditSession): StateChangeOption = {
    val in    = session.selectionText()
    val addNL = adjustNL && (in!="" && in.last!='\n')
    val oldSelection           = session.selection             // get the polarity right on undo
    var out:     Option[String] = None
    var caught:  Boolean       = true

    try {
      out = transform(if (addNL) in+"\n" else in)
    }
    catch {
      case exn: Exception => handle(exn)
    }

    if (out.isEmpty)
      None
    else
    Some {
      val result       = out.get
      val oldSelected  = session.exch(if (addNL) result.init else result)
      new StateChange {
        def undo(): Unit = { session.exch(oldSelected); session.selection = oldSelection }
        def redo(): Unit = session.exch(if (addNL) result.init else result)
        override val kind: String = thisTransform.kind
      }
    }
  }
}

object Filter extends Logging.Loggable {
  val warnings: Notifier[(String, String)] = new Notifier[(String, String)] {}

  /** Run a POSIX process on the supplied input and yield its saved output
   *  -- including its error output -- as a string. Notify handlers of
   *  an erroneous exit code, or an exception, via `warnings`.
   */
  def runProcess(process: ProcessBuilder, input: String): String =
  { val saved = new StringBuilder()
    def save(line: String): Unit = { saved append line; saved append '\n'}
    val logger = ProcessLogger(save, save)
    try {
      val exit = (process #< inputStreamOf(input)).!(logger)
      if (exit!=0) {
        Filter.warnings.notify("External Pipe", s"$process [exit $exit]\n(original input cut)")
      }
      saved.toString()
    }
    catch{
      case exn: Exception =>
        warnings.notify("External Pipe", s"$process ($exn)")
        input
    }
  }
}