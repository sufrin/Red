package Red

import Jed.EditSession

import scala.swing.Dialog

/**
 * A top-level frame that reacts to system requests to close.
 *
 * When such requests are made -- by closing the window frame,
 * or otherwise -- the user is given the choice,  when appropriate,
 * of vetoing the request, honoring the request after any changes
 * to the document being edited have been changed, or honoring it
 * without saving document changes.
 */

abstract class ControllingFrame(theSession: EditSession) extends swing.MainFrame {
  import ControllingFrame._

  /** Save the entity being edited */
  def saveAs(aPath: String): Boolean

  /**
   * Post a dialogue that requires the user to choose
   * one among up to three alternatives, each of which
   * corresponds to a caption on a button
   *
   * @param from the dialogue window title
   * @param message the dialogue message
   * @param choices Up to three `(Caption, Answer)` pairs.
   *                The button with the first caption is pre-selected.
   * @return the answer corresponding to the choice made. The choice
   *         is deemed to be the pre-selected one if the dialogue window
   *         is closed during the dialogue.
   *
   * This is an intellectually tractable `facade` over the toolkit's
   * `Dialogue.showOptions` method
   */
  def showAlternatives[A](from: String, message: String, choices: (String, A)*): A = {
    import swing.Dialog.Message._
    val layout = choices.reverse
    val index  = Dialog.showOptions(
      this, message, from,
      messageType = Question,
      icon=Jed.Utils.redIcon,
      entries=layout.map(_._1),     // the button captions
      initial=layout.length-1).id   // the ordinal of the button pressed

    val answer = if (index<0) layout.last._2 else layout(index)._2  // the answer
    fine (s"showAlternatives(${layout.mkString(",")}) => $index $answer)")
    answer
  }

  locally {
    // Tell the system that the program itself will decide exactly what to do when this
    // window is being asked to close.
    //
    // The overridden `closeOperation` (defined below)  makes the decision.
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
  }

  /**
   *   Invoked by `swing` when this top-level main frame window
   *   is being asked to close, usually because someone clicked
   *   the close button on a window frame.
   *
   *   Our objective in retaining control here
   *   is to avoid nasty surprises,
   *   by asking the user what to do
   *   if the state of the application associated with
   *   the window has changes that need to be saved.
   *
   *   '''OS/X:'''
   *   This is also invoked if Meta-Q (quit) is pressed,
   *   when the application-as-a-whole has the focus, or
   *   if the `Close` button on its dock menu is clicked,
   *   or if the system as a whole is going through a
   *   closedown.
   *   TODO: the present implementation is not very cooperative
   *          with the system in the last case, unless some quite
   *          subtle measures are taken. The effect is to
   *          veto the closedown (rather than saving state
   *          and permitting it).
   */
  override def closeOperation(): Unit = {
    object Choice extends Enumeration { val Save, CloseNow, Cancel = Value }
    if (logging) info("GUI Window Closing")
    // check whether the theSession's file needs to be saved
    val answer  =
      if (theSession.hasChanged)
        showAlternatives(
          from    = "Closing Window",
          message = "Do you really want to stop editing this document?",
          choices = "Keep on editing"              -> Choice.Cancel,
          "Stop editing (do not save)"   -> Choice.CloseNow,
          "Stop editing (after saving)"  -> Choice.Save
        )
      else
        Choice.CloseNow

    if (logging)
      info(s"GUI Closing Window Answer: $answer ${theSession.countTagged("GUI")}")

    answer  match {
      case  Choice.Cancel     => /* keep editing */
      case  Choice.CloseNow   => /* stop editing */
          sessionClose()
      case  Choice.Save       => /* close after successfully saving */
          if (saveAs(theSession.path)) sessionClose()
    }

  }

  def sessionClose(): Unit = {
    // Sever this GUI's connection with `theHostSession`
    theSession.removeTagged("GUI")
    // But other GUI's may be observing (not yet implemented)
    if (theSession.countTagged("GUI") == 0) {
      // close the window
      this.close()
      // tell anyone else who wants to know
      sessionClosed.notify(theSession.name)
    }
  }

  locally {
    peer.setLocationByPlatform(true)  // Magic to place the top level window sensibly
  }

  /** Sends a notification that this top level window has been closed. */
  val sessionClosed = new Notifier[String]

}

object ControllingFrame extends Logging.Loggable
