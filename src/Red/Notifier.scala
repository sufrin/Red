package Red

/**  Subject/Observer infrastructure.
  *
  *   In this adaptation of the subject/observer pattern, a subject
  *   notifies its observers of significant events via a typed
  *   (channel-like) `Notifier[Event]`.
  *
  *    {{{
  *     class EditingSession(val document: DocumentInterface) {
  *           val docChanges = new Notifier[DocumentChanged] {}
  *           val cursorChanges  = new Notifier[CursorChanged] {}
  *           ...
  *           docChanges.notify(DocumentChangedAt(cursor, extent))
  *           ...
  *           cursorChanges.notify(CursorChanged(39, 45))
  *           ...
  *   }}}
  *
  *  A handler for events of type `Event` is a (partial) function
  *  -- essentially of type "Event=>Unit", that may not be defined
  *  for all values of type `Event`.
  *
  *  An observer associates one or more typed "handlers" for events
  *   with one or more  `Notifier`s. When a notifier's notify method
  *   is invoked with an event, the handlers associated with the
  *   notifier are applied to the event.
  *
  * For example
  *  {{{
  *     class DocumentView(theSession: EditingSession)
  *           ...
  *           theSession.docChanges.handleWith {
  *              case DocumentChangedAt(...)  => ...
  *           }
  *           ...
  *           theSession.cursorChanges.handleWith {
  *              case CursorChanged(row, col) => ...
  *              case SelectionChanged(...) => ...
  *           }
  *           ...
  *
  *    class GUI(theSession: EditingSession)
  *          ...
  *          theSession.cursorChangedhandleWith {
  *             case CursorChanged (row, col) =>
  *                 feedbackLine.write(s" ... (\$row,\$col)")
  *          }
  *          ...
  *  }}}
  */

class Notifier[Event] (name: String = "Anonymous") {
  import Notifier.Handler

  private val handlers =
    new collection.mutable.ListBuffer[(AnyRef, Handler[Event])]

   /** Notify `event` to each handler associated with
    *  this notifier. If, and only if, that handler "fails" (because
    *  it is not defined at that event) the event is
    *  passed, together with the notifier's name and handler's tag,
    *  to the system-wide "catch-all" handler, `Notify.unHandled`.
    */
  def notify(event: Event): Unit = {
    for ((tag, handler) <- handlers) {
        handler.applyOrElse( event, Notifier.unHandled(name, tag)(_)  )
    }
  }

  /** Offer `event` to all handlers, and return true if any of them succeeded */
  def anyHandled(event: Event): Boolean = {
    var succeed = false
    for ((_, handler) <- handlers) {
      try   { handler.apply(event); succeed = true }
      catch {
        case _: scala.MatchError => ()
      }
    }
    succeed
  }

  /** Offer `event` to all handlers, and return true if none of them failed  */
  def allHandled(event: Event): Boolean = {
    var succeed = true
    for ((_, handler) <- handlers) {
      try   { handler.apply(event) }
      catch {
        case _: scala.MatchError => succeed = false
      }
    }
    succeed
  }

  /** Associate the `handler: Handler`, with tag `tag`,  with this
   *  `Notifier`.
   *
   *  Tags can be of any type: they are used to identify
   *  categories of `Handler` for purposes such as
   *  reference-counting and removal.
   *
   *  '''Note''' In some architectures
   *  it make sense to know (of a resource-holding object
   *  notifying events through a `Notifier`) how many other objects
   *  are interested in the kind of event it is notifying. When the
   *  number of interested objects reaches zero, the "notifying"
   *  object is told that it can release the resources it holds.
   *
   */
  def handleWithTagged(tag: AnyRef)(handler: Handler[Event]): Unit = {
      if (handlers.isEmpty) enableResources()
      handlers += ((tag, handler))
  }

   /** Associate the `handler: Handler`, with tag `None`,  with this
   *  `Notifier`. The `None` tag is as good as any other.
   */
  def handleWith(handler: Handler[Event]): Unit =
      handleWithTagged(None)(handler)

  /**  The number of associated handlers with the given `tag` */
  def countTagged(tag: AnyRef): Int = handlers.count {
    case (aTag, _) => aTag eq tag
  }

  override def toString: String =
    handlers.mkString(s"Notifier($name)(", ",", ")")

  /**  Remove all associated handlers with the given `tag` */
  def removeTagged(tag: AnyRef): Unit = {
    handlers.filterInPlace { case (aTag, _) => !(aTag eq tag) }
    if (handlers.isEmpty) disableResources()
  }

   /** Invoked as the first handler is declared for this notifier.
    *  Intended to commit notifier-specific resources necessary
    *  for the functioning of this notifier.
    */
   protected def enableResources():  Unit = {}
   /** Invoked after the last handler for this notifier is removed.
    *  Intended to release notifier-specific resources that were necessary
    *  for the functioning of this notifier.
    */
   protected def disableResources(): Unit = {}

   /**
    * A derived handler that is enabled only for the duration of a command
    */
   def handleOnce(tag: AnyRef)(handler: Handler[Event])(body: => Unit): Unit = {
     handleWithTagged(tag)(handler)
     try     { body }
     finally { removeTagged(tag) }
   }
 }

 /**
  * Defines `Handler[Event]`.
  *
  * Defines methods for declaring exactly
  * what happens if any of any notifier's associated handlers
  * fails to handle a notified event. In general this should
  * be considered non-ignoreable, but the manner of its
  * reporting can vary between (whole) programs.
  *
  * A notifier that has no associated handlers at all is, of course,
  * permissible.
  */
 object Notifier {
  type Handler[Event] = PartialFunction[Event, Unit]

  /** Insist on the type of the given `handler`: a type coercion to
   *  overcome a slight difficulty with Scala 2.13 type inference.
   */
  def Handler[Event](handler: Handler[Event]): Handler[Event] = handler

 /** Ignore a report  */
  private def ignoreUnhandled(report: => String): Unit = ()

  private var _unHandled: (=> String)=>Unit = ignoreUnhandled(_)

  private [Notifier]
  def unHandled(handlerName: String, tag: AnyRef)(event: Any): Unit =
    _unHandled (s"Handler tagged $tag failed to handle $handlerName.notify($event) ")

  /** Henceforth unmatched notifications throw an `IllegalArgumentException`.
   */
  def exceptionUnhandled(): Unit =
    _unHandled = {
      report => throw new IllegalArgumentException( report )
    }

  /** Henceforth unmatched notifications generate
   *  a stack trace on the `System.err` console.
   *  This is the initial configuration.
   */
  def stackTraceUnhandled(): Unit =
    _unHandled = {
      report  =>
         new IllegalArgumentException(report).printStackTrace()
    }

  /** Henceforth unmatched notifications are completely ignored */
  def ignoreUnhandled(): Unit = _unHandled = ignoreUnhandled

  /** Henceforth unmatched notifications are reported on the `System.err` console */
  def reportUnhandled(): Unit =
    _unHandled = System.err.println

  locally {
    stackTraceUnhandled()
  }
 }
