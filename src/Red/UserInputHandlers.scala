package Red

object UserInputHandlers {
  type UserInputHandler = Notifier.Handler[UserInput]

  /** A handler that invokes the computation `act` in response to a newline */
  def onNewline(act: => Unit): UserInputHandler = {
    case Character('\n', _, _) => act
  }

  /** A handler that logs every input event, with `log.finest` then fails.
   *  {{{
   *    logAll(log) orElse handler
   *  }}}
   *  behaves like `handler` but also logs input events with `log.finest`.
   */
  def logAll(title: String="", log: Logging.Logger = Logging.Default.log): UserInputHandler = {
    case what: UserInput if {
      log.finest(s"$title $what")
      false
    } => ()
  }

}
