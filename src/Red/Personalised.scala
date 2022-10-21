package Red

import RedScript.Language.{Const, Nothing, RuntimeError, SExp, SExps, Str, Variable}
import RedScript.{Env, Evaluator, Language}
import Useful.PrefixMap

import java.nio.file.{Path, Paths}
import scala.swing.Dialog

/**
 *  Personalisation module, with definitions of
 *  abbreviations, menu entries, etc.
 *
 *  TODO: One-per-editsession?
 *
 */
object Personalised extends Logging.Loggable {

  /** Block types to be placed on a `Latex` menu */
  val personalBlockTypes =  new collection.mutable.ListBuffer[String]
  /** Program names to be placed on the `Pipe` menu */
  val personalPipeNames = new collection.mutable.ListBuffer[String]

  def latexBlockTypes: Seq[String] =
  { val default = "foil itemize enumerate - note exercise answer - code -code code* scala alltt - center verbatim comment smaller - question part ans"
    Bindings.importBindings()
    if (personalBlockTypes.isEmpty)
       FilterUtilities.parseArguments(sys.env.getOrElse("REDLATEXBLOCKS", default))
    else
    personalBlockTypes.toList
  }

  def pipeNames: Seq[String] =
  { val default = "wc; ls -lt; printenv"
    Bindings.importBindings()
    if (personalPipeNames.isEmpty)
      sys.env.getOrElse("REDPROGRAMS", default).split(";[ ]+").toList
    else
      personalPipeNames.toList
  }

  def clearBindings(): Unit = {
    personalPipeNames.clear()
    personalBlockTypes.clear()
    Bindings.clearMapping()
    Bindings.RedScriptEvaluator.clear()
  }

  def profileWarning(message: String): Unit = warning(s"", message)

  def warning(heading: String, message: String): Unit = {
    Dialog.showMessage(
      null,
      s"$message${if (heading.nonEmpty) s"\n$heading" else ""}",
      "Bindings",
      Dialog.Message.Warning,
      icon = Utils.redIcon
    )
  }

  object Settings {
    var typeOverSelection: Boolean = true
    var clickSelects: Boolean = true
    var autoIndenting: Boolean = true
  }


  object Bindings {
    val feedback: Notifier[String] = new Notifier[String]("Personalised Feedback")

    val profileChanged: Notifier[String] = Features.profileChanged

    private val trie = PrefixMap[String]()
    /** Modification time of the last root bindings file */
    var lastImportTime: Long = 0

    def profile: String = Features.profile

    def clearMapping(): Unit = trie.clear()

    def longestSuffixMatch(chars: CharSequence, upTo: Int): Option[(String, Int)] = {
       importBindings()
       trie.longestSuffixMatch(chars, upTo)
    }

    /** Add an abbreviation mapping */
    def addMap(abbrevs: (String, String)*): Unit =
        for { (abbrev, result) <- abbrevs } mapTo(abbrev, result)

    def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(abbrev, result)

    /** Add a single cycle of abbreviations */
    def addCycle(abbrevs: String*): Unit = {
      for { i<-0 until abbrevs.length -1 } trie.reverseUpdate(abbrevs(i), abbrevs(i+1))
    }

    def toPath(context: Path, path: String): Path =
      if (path.startsWith("~/"))
        java.nio.file.Paths.get(Red.Utils.expandHome(path))
      else {
        val thePath = java.nio.file.Paths.get(path)
        if (thePath.isAbsolute)
          thePath
        else
          context.resolveSibling(thePath)
      }

    object RedScriptEvaluator extends Evaluator {

      def clear(): Unit = global.clear()

      val paths = new collection.mutable.Stack[Path]

      override def readEvalPrint(path: Path, show: Boolean): Unit = {
          paths.push(path)
          super.readEvalPrint(path, show)
          paths.pop()
      }

      def doInclude(args: List[Const]): Const = {
        val (newPath, show) = args match {
          case Str(newPath)::Nil                       => (newPath, false)
          case List(Str(newPath), Language.Bool(show)) => (newPath, show)
        }
        val exPath = toPath(paths.top, newPath)
        if (exPath.toFile.exists() && exPath.toFile.canRead())
          importBindings(1+paths.length, paths.top, exPath, show)
        else
          profileWarning(s"Error including:  $exPath)\nFrom           : ${position}\nNon-existent path or unreadable file.")
        Nothing
      }

      def doAlt(shifted: Boolean)(args: List[Const]): Const = {
        for { Str(map) <- args }  AltKeyboard.mapTo(map(0).toUpper, map(1), shifted)
        Nothing
      }

      def doPopup(message: List[Const]): Const = {
        val lines = message.map(_.toString).mkString("", "\n", "")

        Dialog.showMessage(
          null,
          lines,
          "Bindings",

          Dialog.Message.Plain,
          icon = Utils.redIcon
        )

        Nothing
      }

      def doDia(strings: List[Const], shifted: Boolean): Unit= {
        for { Str(mark) <- strings }  AltKeyboard.macKeyboardDiacritical = mark
      }

      def doFont(env: Env, params: SExp) : Const = {
        val SExps(args) = params
        args match {
          case family :: style :: size :: roles =>
            // PRO-TEM: inherit the old machinery
            // def setFont(_kind: String, style: String, _size: String, roles: Seq[String])
            val roleNames = for {case Variable(role) <- roles} yield role
            try
              println("SETFONT", family.eval(env).toString, style.eval(env).toString, size.eval(env).toString, roleNames)
            catch {
              case exn => exn.printStackTrace()
            }
            Utils.setFont(family.eval(env).toString, style.eval(env).toString, size.eval(env).toString, roleNames)
          case _ => throw RuntimeError(s"Malformed font specification: $args")
        }
        Nothing
      }

      import Language._
      val bindingPrimitives: List[(String, Const)] = List(
        "abbrev"      -> Subr("abbrev",      {  case List(Str(abbr), Str(text)) => mapTo(abbr, text); Nothing }),
        "diacritical" -> Subr("diacritical", {  case List(Str(marks)) => AltKeyboard.macKeyboardDiacritical = marks; Nothing }),
        "altclear"    -> Subr("altclear",    {  Nil => AltKeyboard.clear(); Nothing }),
        "altplain"    -> Subr("altplain",   doAlt(false)(_)),
        "altshift"    -> Subr("altshift",   doAlt(true)(_)),
        "include"     -> Subr("include",    doInclude(_)),
        "popup"       -> Subr("popup",      doPopup(_)),
        "font"        -> FSubr("font",      { case (env, params) => doFont(env, params)}),
      )
      locally {
        for { (name, value) <- bindingPrimitives } syntaxEnv.define(name, value)
      }
    }

    /** Read the top-level bindings file if it has been updated since it was last read */
    def importBindings(): Unit = {
      val path = sys.env.getOrElse("REDBINDINGS", "~/.red/bindings.redscript")
      val top = Paths.get("")
      try importBindings(0, top, toPath(top, path)) catch {
        case AbortBindings(why) => profileWarning(why)
      }
      Features.sync()
    }

    /** Read the top-level bindings file (unconditionally) */
    def reImportBindings(): Unit = {
      lastImportTime = 0
      clearBindings()
      importBindings()
    }

    def importBindings(depth: Int, context: Path, path: Path, show: Boolean = false): Unit = {

    /**
     *  Read a single preferences file:
     *  @param depth the depth of include-nesting (with an ''ad-hoc'' limit to detect cyclic includes)
     *  @param context the path to the file being read
     */
      val file = path.toFile
      val timeStamp = if (file.exists) file.lastModified() else 0

      def readFile(): Unit = {
        if (logging) info(s"importing bindings from: $file")
        RedScriptEvaluator.readEvalPrint(path, show)
      }


      if (file.exists()) {
         if (depth==0) {
           // re-read root if necessary
           if (timeStamp>lastImportTime) {
              clearBindings()
              readFile()
              lastImportTime = timeStamp
           }
         }
         else
         // read nested files if they are not nested too deeply (and there's no cycle)
         if (depth<=6)
           readFile()
         else
           throw AbortBindings(s"Attempting, from $context, to include:\n $path\nThis bindings file forms a cycle or is nested too deeply (>6)")
      } else
        if (depth==0)
           throw AbortBindings(s"No bindings file $path   (this is not catastrophic)\nEither copy AppleRed.app/Contents/Resources/Bindings to ~/.red\nor export REDBINDINGS=...a bindings file...")
        else
           throw AbortBindings(s"Attempting, from $context, to include:\n $path\nThere is no such bindings file\n")

    }

    case class AbortBindings(cause: String) extends Error(cause)

  }
}
