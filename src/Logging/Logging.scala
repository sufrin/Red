import java.io.PrintStream

/**
 * A conceptually simple logging utility, requiring minimal setup.
 *
 * It is, of course, always easy to splatter `println` calls
 * around one's program: but this component supports discrimination
 * between levels, and dynamic setting (at run-time) rather
 * straightforwardly.
 *
 * @see Logging.Loggable for examples of its use.
 */
package object Logging {
  /** If `stream` is non-null then run `body` with `stdout` and `stderr` set to `stream`. */
  def withConsole(stream: java.io.PrintStream = null)( body : => Unit): Unit = {
      if (stream==null) {
        body
      }
      else
        withOut (stream) { withErr (stream) { body } }
  }

  /**
   * Construct a printstream suitable for redirecting `stdin` and/or `stdout`.
   *
   * @param filePath path to the file to which the resulting stream is directed
   * @param mustExist if true this causes `null` to be returned unless there is a writeable file at `filePath`
   * @param append if true this causes the destination file to be appended
   * @return a stream directed at `filePath`, or `null` if `mustExist` and there is no writeable file at `filePath`
   */
  def logStream(filePath: String, mustExist: Boolean = true, append: Boolean = true): java.io.PrintStream = {
    import java.nio.file.StandardOpenOption._
    import java.nio.file._
    val path = Paths.get(filePath)
    if (mustExist && !(path.toFile.exists() && path.toFile.canWrite())) {
      Logging.Default.warn(s"Logfile $path should exist and be writeable")
      null
    } else {
      val stream = if (append) Files.newOutputStream(path, CREATE, APPEND) else Files.newOutputStream(path, CREATE)
      new PrintStream(stream)
    }
  }

  /**
   * 1. If `stream` is non-null then run body with `stderr` set to `stream`,
   *    otherwise just run `body`.
   *
   * 2. In both cases: on termination or exception revert to the original `stderr`.
   */
  def withErr(stream: java.io.PrintStream)( body : => Unit): Unit =
  { val err = System.err
    if (stream!=null) System.setErr(stream)
    try { body } finally { System.setErr(err) }
  }

  /**
   * 1. If `stream` is non-null then run body with `stdout` set to `stream`,
   *    otherwise just run `body`.
   *
   * 2. In both cases: on termination or exception revert to the original `stdout`.
   */
  def withOut(stream: java.io.PrintStream)( body : => Unit): Unit =
  { val out = System.out
    if (stream!=null) System.setOut(stream)
    try { body } finally { System.setOut(out) }
  }

  case class LogMessage(level: Int, logName: String, message: String)

  /** @param logName        the name of the log, transmitted with all messages.
    *
    *   @param logLevel       the threshold at and above which messages
    *                         sent to this log will be written. The higher
    *                         the value of `logLevel`, the '''less transmissive'''
    *                         the logger is. In particular, the level `OFF`
    *                         (the highest integer)
    *                         suppresses transmission of all messages, and
    *                         the level `ALL` enables transmission of all messages.
    *
    * @param writeMessage     the function used to write messages: this can be reassigned
    *                         after the log is constructed.
    *
    * @param host             the `Loggable` (if any) that owns this log.
    *                         Symbolic names for levels are, in descending order,
    * {{{
    * val OFF      = Integer.MAX_VALUE
    * val UNSET    = OFF-1
    * val FATAL    = 50000
    * val ERROR    = 40000
    * val WARN     = 30000
    * val INFO     = 20000
    * val FINE     = 10000
    * val FINER    = 5000
    * val FINEST   = 4000
    * val ALL      = Integer.MIN_VALUE
    * }}}
    */
  class Logger(
      logName: String,
      logLevel: Int,
      var writeMessage: LogMessage => Unit,
      var host: Option[Loggable] = None
  ) {

    @inline def writeLog(level: Int, logName: String, message: String): Unit =
      writeMessage(LogMessage(level, logName, message))

    /** send a message to this log at level `FINEST` */
    def finest(message: => String): Unit =
      if (level <= FINEST) writeLog(FINEST, logName, message)

    /** send a message to this log at level `FINER` */
    def finer(message: => String): Unit =
      if (level <= FINER) writeLog(FINER, logName, message)

    /** send a message to this log at level `FINE` */
    def fine(message: => String): Unit =
      if (level <= FINE) writeLog(FINE, logName, message)

    /** send a message to this log at level `INFO` */
    def info(message: => String): Unit =
      if (level <= INFO) writeLog(INFO, logName, message)

    /** send a message to this log at level `WARN` */
    def warn(message: => String): Unit =
      if (level <= WARN) writeLog(WARN, logName, message)

    /** send a message to this log at level `ERROR` */
    def error(message: => String): Unit =
      if (level <= ERROR) writeLog(ERROR, logName, message)

    /** send a message to this log at level `FATAL` */
    def fatal(message: => String): Unit =
      if (level <= FATAL) writeLog(FATAL, logName, message)

    /** send a message to this log at level `level` */
    def log(messageLevel: => Int, message: => String): Unit =
      if (level <= messageLevel) writeLog(messageLevel, logName, message)

    private var _level: Int = _

    locally {
      level = logLevel
    }

    /** Set the logging level of this logger.
      * The `sys.props` value of `s"logName.level"` is made consistent with this level.
      * If there's a host, then its `logging` is set true iff `newLevel!=OFF`.
      */
    def level_=(newLevel: Int): Unit = {
      _level = newLevel
      sys.props.put(s"$logName.level", levelName(newLevel))
      for { h <- host } h.logging = newLevel != OFF
    }

    def level: Int = _level

    override def toString: String =
      s"Logger($logName, ${levelName(level)}, ${host.getOrElse(None).getClass.getName})"

  }

  /** Named logging thresholds: the higher the threshold the less is logged */
  val OFF: Int = Integer.MAX_VALUE
  val UNSET: Int = OFF - 1
  val FATAL: Int = 50000
  val ERROR: Int = 40000
  val WARN: Int = 30000
  val INFO: Int = 20000
  val FINE: Int = 10000
  val FINER: Int = 5000
  val FINEST: Int = 4000
  val ALL: Int = Integer.MIN_VALUE

  /** Mapping from names to levels and vice-versa. This is mutable so that new levels can be named.
    */
  private val _levelName: collection.mutable.Map[Int, String] =
    collection.mutable.HashMap(
      OFF -> "OFF",
      UNSET -> "UNSET",
      FATAL -> "FATAL",
      ERROR -> "ERROR",
      WARN -> "WARN",
      INFO -> "INFO",
      FINE -> "FINE",
      FINER -> "FINER",
      FINEST -> "FINEST",
      ALL -> "ALL"
    )

  /**  Public interface to the level name table.
    *  It may be that in a specific program, for the purposes
    *   of analyzing a log in detail, it's necessary to distinguish
    *   between several levels that have nearly the same value but are
    *   suppressed/permitted together. This can be done by updating
    *   `Loggable.levelName`, then using the generic `log` function.
    *
    *   It may be convenient to capture the numeric level
    *  as a constant value at the same time as assigning it a
    *  name. For example:
    *
    *  {{{
    *     val SYMBOLS = levelName(FINEST+1) = "SYMBOLS"
    *     val DISTAFF = levelName.update(FINEST+2, "DISTAFF")
    *     ...
    *     log(SYMBOLS,  s"added \$symbol to the symbol table")
    *     log(DISTAFF, s"\$m is \$n"
    *  }}}
    */
  object levelName {
    def update(levelValue: Int, levelString: String): Int = {
      _levelName.update(levelValue, levelString)
      if (levelString.length > levelNameFieldWidth) {
        levelNameFieldWidth = levelString.length
        _logMessageFormat = logMessageFormat
      }
      levelValue
    }

    def apply(level: Int): String = _levelName.getOrElse(level, level.toString)
  }

  /** Map symbolic string (or digit sequence) to integer logging level */
  def toLogLevel(level: String): Int = _levelName.find {
    case (_, v) => v == level.toUpperCase
  } match {
    case None =>
      assert(level.matches("[0-9]+"),
             s"Logging.toLogLevel($level): malformed level specification")
      level.toInt
    case Some((name, _)) => name
  }

  /**  The default method for writing log messages that is installed in `Logger`s at the time
    *   of their creation
    */
  var defaultWriteLogMessage: LogMessage => Unit = {
    case LogMessage(level: Int, logName: String, message: String) =>
      scala.Console.println(
        logMessageFormat.format(levelName(level), logName, message)
      )
  }

  /** Mapping from the names of logs to the corresponding logs */
  private val logTable = new scala.collection.mutable.HashMap[String, Logger]()

  /** size of the longest logName seen so far: minimum is 20 */
  private var logNameFieldWidth: Int = 20

  /** size of the longest levelName seen so far: minimum is 6 */
  private var levelNameFieldWidth: Int = 6

  /** the current log message format: used to format `(levelName, logName, message)` */
  protected var _logMessageFormat: String = logMessageFormat

  /** Compute the current log message format based on the maximum lengths of `levelName` and `logName` seen so far.
    *  The idea is to align log output in readable columns (as far as possible) once the various loggers have
    *  been set up.
    */
  protected def logMessageFormat: String =
    s"%${levelNameFieldWidth}s@%-${logNameFieldWidth}s: %s"

  /**  Return a ''unique'' `Logger` with the given `logName`.
    */
  def Logger(
      logName: String = "",
      logLevel: Int = WARN,
      writeLog: LogMessage => Unit = defaultWriteLogMessage,
      host: Option[Loggable] = None
  ): Logger = logTable.get(logName) match {
    case None =>
      val theLog = new Logger(logName, logLevel, writeLog, host)
      logTable.put(logName, theLog)
      if (logName.length > logNameFieldWidth) {
        logNameFieldWidth = logName.length
        _logMessageFormat = logMessageFormat
      }
      theLog
    case Some(theLog) =>
      theLog.host = host
      theLog.level = theLog.level // enables logging if the level is right
      theLog
  }

  /** Set the log level of the logger named `logName` to `logLevel`, making a new
    * logger (with default writer and no host) if there is not already a logger
    * with the given name.
    */
  def update(logName: String, logLevel: String): Logger =
    logTable.get(logName) match {
      case None =>
        Logger(logName, toLogLevel(logLevel))
      case Some(theLog) =>
        theLog.level = toLogLevel(logLevel)
        theLog
    }

  /** A convenience trait to support efficient use of
    * logging from outside an object whose logging calls
    * are written idiomatically as (for example)
    *
    * {{{
    *   if (logging) finer(...)
    *   if (logging) info(...)
    * }}}
    *
    * If a (companion) object `KLARSE` is declared to extend
    * {{{    Logging.Loggable}}}
    * its own specific `Logger` is constructed, also named `"KLARSE"`
    * whose initial level is by default `WARN` but can be specified by a system
    * property
    * {{{    KLARSE.level}}}
    * at the serveWith of  the `scala` run of the program.
    *
    * The class itself should
    * {{{    import KLARSE._}}}
    * so that the various logging methods are in scope.
    *
    * Logger messages are written by the method
    * {{{    KLARSE.writeLogMessage: LogMessage=>Unit }}}
    * whose default value writes suitably-formatted messages to the
    * console error stream. This may be overridden as the `Loggable`
    * companion object is constructed.
    *
    * Here's a small example
    * {{{
    *  class LogEg {
    *  import LogEg._
    *  def foo(x: String): Unit = if (logging) info(s"foo(\$x)")
    * }
    *
    * object LogEg extends Logging.Loggable
    * }}}
    *
    * Notice that the class imports everything from its companion object
    * though it need only inherit `logging`, and `info` here.
    *
    * The logging level for objects can be set
    * in several ways.
    *
    * As a (Java) property at the scala command line, with (eg)
    * {{{
    *   scala -DLogEg.level=FINEST ....
    * }}}
    *
    * or an assignment (in the program) of the form
    *
    * {{{
    *   LogEg.level = Logging.NONE
    * }}}
    *
    * Or symbolically, while the program is running, with
    * (eg) a call of the form
    * {{{
    *   Logging("LogEg") = "ALL"
    * }}}
    *
    * The integration of string matching into the `match` construct means that
    * the latter can easily be done while processing the arguments of a
    * command-line program.
    *
    * For example: here's part of an editing program:
    * {{{
    *   def main(args: Array[String]): Unit =
    *       for { arg <- args }
    *         arg match
    *         {  case s"-l\${module}=\${level}" => Logging.update(module, level)
    *            case _                       => editTheFile(arg)
    *         }
    * }}}
    *
    * But you don't need to declare any `Loggable` entities to
    * do some elementary logging. `Logging.Default` is predefined as a `Loggable`
    * object  whose methods can be used directly, and whose default
    * initial level, unless set as a system property, is `ALL`.
    *
    * {{{
    *   import Logging.Default._
    *   ...
    *   if (logging) warn("We are logging directly, using the default log")
    *   if (logging) trace(s"We are logging directly, using the default log")
    * }}}
    */
  trait Loggable {

    /** The path name of this class: shorn of its
     *   trailing '$' if it's a companion object,
      *  and with internal '$' transformed into '.'.
      *
      *  This name can be overridden when a custom `className` is wanted.
      *
      *  @see LogTest
      */
    def className: String = {
      val n = this.getClass.getName
      (if (n.endsWith("$")) n.substring(0, n.length - 1) else n)
        .replace('$', '.')
    }

    /** Key to the logging level of this class in `sys.props`.
      *
      * Its value is  `s"\${className}.level"`
      */
    val classKey = s"$className.level"

    /** The initial logging level of this class, as specified
     *  in `sys.props`, otherwise `UNSET`
      */
    val initialLevel: Int =
      toLogLevel(sys.props.getOrElse(classKey, "UNSET"))

    /** Set the level of the log associated with this class */
    def level_=(newLevel: Int): Unit = log.level = newLevel

    /** The level of the log associated with this class */
    def level: Int = log.level

    /**  Are we logging on this log. Ideally this would be
      *  a function derived as shown: but my intuition
      *  tells me that this could be expensive. So once
      *  the log has been associated with its host object
      *  we establish the ''invariant''
      *  {{{ logging == log.level != Logger.OFF }}}
      */
    var logging = false // eventually invariantly = log.level != Logger.OFF

    /** The logger associated with this class or object.
      *
      * '''NB''' until very recently (Dec 10 2021) it was not constructed
      * until the first use of one of its methods; but that seemed (!) to cause
      * difficulties in enabling logging of classes composed with
     *  logged ''traits''.
      * //TODO: Investigate
      */
    /*lazy*/
    val log: Logger =
      Logger(className, initialLevel, defaultWriteLogMessage, Some(this))

    /** Invoke this method's namesake on the `log` */
    def finest(message: => String): Unit = log.finest(message)

    /** Invoke this method's namesake on the `log` */
    def finer(message: => String): Unit = log.finer(message)

    /** Invoke this method's namesake on the `log` */
    def fine(message: => String): Unit = log.fine(message)

    /** Invoke this method's namesake on the `log` */
    def info(message: => String): Unit = log.info(message)

    /** Invoke this method's namesake on the `log` */
    def warn(message: => String): Unit = log.warn(message)

    /** Invoke this method's namesake on the `log` */
    def error(message: => String): Unit = log.error(message)

    /** Invoke this method's namesake on the `log` */
    def fatal(message: => String): Unit = log.fatal(message)

    /** Invoke this method's namesake on the `log` */
    def log(level: => Int, message: => String): Unit = log.log(level, message)

    override def toString =
      s"Loggable: $className initialLevel=$initialLevel, level=level, log=$log"

  }

  /** The default `Loggable` object. It can be used with no preliminary
    *  rituals, in which case its level is taken to be `ALL`. Its level
    *  can also be set by any of the usual methods. For convenience
    *  it is known as: "Logging.Default" to all of the level-setting
    *  methods.
    *
    *  As the program is starting
    *  {{{
    *   scala -DLogging.Default.level=FINEST ...
    *  }}}
    *
    *  By symbolic assignment
    *  {{{
    *   Logging("Logging.Default")="FINEST"
    *  }}}
    *
    *  By assignment
    *  {{{
    *   Logging.Default.level=Logging.FINEST
    *  }}}
    */
  object Default extends Loggable {
    override def className: String = "Logging.Default"
    locally {
      if (level == UNSET) level = ALL
    }
  }

}
