package Red

import scala.collection.mutable

/**  The `CutRing` keeps the most recent cuts/deletions
  *  from editing sessions that have installed the
  *  `CutRing.Plugin` trait. It merges spatially+temporally
  *  adjacent cuts.
  */
object CutRing extends Logging.Loggable {

  /** A plugin that adds CutRing functionality to an `EditEditSessionInterface` by
    *  implementing a substantive method to record material cut from the
    *  session's document.
    */
  trait Plugin extends EditEditSessionInterface {
    override def hasCutRing: Boolean = true

    /** The merged aggregate of the most recent sequence of adjacent recorded cuts. */
    var aggregatedCuts: Cut = emptyCut

    /**   Record a cut; merging it with the current aggregate if possible, and
      *   adding the state of the aggregate to the cut ring.
      */
    override def recordCut(thisCut: Cut): Unit = {
      if (logging) finer(s"CutRing.Plugin: $thisCut")
      aggregatedCuts = aggregatedCuts merge thisCut
      CutRing.addCut(aggregatedCuts)
      if (logging) finer(s"CutRing.Plugin [merged]: $aggregatedCuts")
    }
  }

  /** The maximum number of entries in the ring */
  var bound = 80

  /** The actual number of entries in the ring: <= `bound` */
  def length: Int = cutRing.length

  /** The empty cut */
  @inline def emptyCut: Cut = Cut("", 0, 0, 0L)

  /** A plugin that adds `CutRing` functionality to an `EditEditSessionInterface` */

  case class TimeStamped(text: String, time: String)

  /**  The cut ring is implemented here as a bounded queue, though it would
    *  more appropriately be kept as a bounded cyclic list.
    *
    *  '''Inv:'''  `queue.length<=bound`
    */
  private val cutRing: mutable.Queue[TimeStamped] =
    new mutable.Queue[TimeStamped]

  /** Add a cut to the ring, but if contains its
    *  predecessor in the ring then replace its
    *  predecessor. This is the way successive
    *  adjacent deletions/cuts end up occupying
    *  a single slot in the ring. This method is
    *  not particularly efficient, but it is
    *  straightforward.
    */
  def addCut(aCut: Cut): Unit = {
    // contains its predecessor
    if (cutRing.nonEmpty && aCut.text.contains(cutRing.last.text))
      cutRing.dropRightInPlace(1) // remove from the end of the queue

    /** Make space, if necessary, by removing the least-recently queued. */
    if (cutRing.length >= bound) cutRing.dequeue()

    cutRing.enqueue(TimeStamped(aCut.text, Utils.dateString()))
    ringChanged.notify(())
  }

  /** Materialize the entire cut ring, in reverse order, as text -- embedding each individual
    *  cut in "««««« ... »»»»»".
    */
  def toText: String = {
    val buf = new StringBuilder()
    for { cut <- cutRing.reverse } {
      buf append s"«««««««« ${cut.time}\n"
      buf append cut.text
      buf append s"\n»»»»»»»»\n"
    }
    buf.toString()
  }

  /** Publish the fact that the cut ring has changed */
  val ringChanged: Notifier[Unit] = new Notifier[Unit] {}
}
