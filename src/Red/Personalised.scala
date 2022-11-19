package Red

import RedScript.Language._
import RedScript.{Env, Evaluator, Language, Parser, SourcePosition}
import Useful.PrefixMap

import java.io.File
import java.nio.file.{Path, Paths}
import scala.swing.{Dialog, Font}

/**
 *  Personalisation module, with definitions of
 *  abbreviations, menu entries, etc.
 *
 *  TODO: "safe"-mode should have a built-in minimal script.
 *
 */
object Personalised extends Logging.Loggable {

  def applyScript(name: String, path: String): SExp = {
    val fname = Variable(name)
    val sexp = SExps(List(fname,  Str(path)))
    sexp.position  = SourcePosition(s"setting up UI for $path")
    fname.position = sexp.position
    Bindings.RedScriptEvaluator.run(sexp)
  }

  def latexBlockTypes(path: String): Seq[String] =
  { applyScript("UI:latexBlockTypes",  path) match {
      case SExps(Nil) => Nil
      case SExps(es)  => es.map(_.toPlainString)
      case other      => profileWarning(s"latexBlockTypes: path -> Seq[String]: $other"); Nil
    }
  }

  def latexClasses(path: String): Seq[(String,String)] =
  { applyScript("UI:latexClasses",  path) match {
    case SExps(Nil) => Nil
    case SExps(es)  => es.map {
      case Pair(button, (Str(text))) => (button.toPlainString, text)
      case Pair(button, (Str(text))) => (button.toPlainString, text)
      case other => profileWarning(s"UI:latexClasses $other"); ("BAD", other.toString)
    }
    case other      => profileWarning(s"latexClasses: path -> Seq[(String, String)]: $other"); Nil
  }
  }

  def needsLatex(path: String): Boolean = {
    Bindings.importBindings()
    applyScript("UI:needsLatex", path) match {
      case Bool(bool) => bool
      case other      => profileWarning(s"needsLatex: path -> Bool: $other"); false
    }
  }

  def needsPandoc(path: String): Boolean = {
    Bindings.importBindings()
    applyScript("UI:needsPandoc", path) match {
      case Bool(bool) => bool
      case other      => profileWarning(s"needsPandoc: path -> Bool: $other"); false
    }
  }

  def pipeShellCommands(path: String): Seq[String] =
  { Bindings.importBindings()
    applyScript("UI:pipeShellCommands", path) match {
      case SExps(es) => es.map(_.toPlainString)
      case other     => profileWarning(s"pipeShellCommands: path -> Seq[String]: $other"); Nil
    }
  }

  def pipeRedScripts(path: String): Seq[String] =
  { Bindings.importBindings()
    applyScript("UI:pipeRedScripts", path) match {
      case SExps(es) => es.map(_.toPlainString)
      case other     => profileWarning(s"pipeRedScripts: path -> Seq[fun]: $other"); Nil
    }
  }


  def clearBindings(): Unit = {
    Bindings.clearMapping()
    Bindings.RedScriptEvaluator.reset()

  }

  def profileWarning(message: String): Unit = { warning(s"", message) }

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
    var clickSelects:      Boolean = true
    var autoIndenting:     Boolean = true
    var insertionSelects:  Boolean = true
  }


  object Bindings {
    val feedback: Notifier[String] = new Notifier[String]("Personalised Feedback")

    val profileChanged: Notifier[String] = Features.profileChanged

    private val trie = PrefixMap[String]()
    /** Modification time of the last root bindings file */
    var lastImportTime: Long = 0

    def profile: String = Features.profile

    def clearMapping(): Unit = trie.clear()

    def findAbbreviation(chars: CharSequence, upTo: Int): Option[(String, Int)] = {
       importBindings()
       trie.longestSuffixMatch(chars, upTo)
    }

    def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(abbrev, result)

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

      case class UserInput(input: Red.UserInput) extends Const
      case class SessionCommand(command: EditSessionCommands.SessionCommand) extends Const

      case class FontExpr(name: String, font: Font) extends Const {
        override def toString: String = s"(font \"$name\")"
      }

      val output = new StringBuilder()

      override def errorFeedback(s: String): Unit    = profileWarning(s) // throw AbortBindings(s)
      override def normalFeedback(s: String): Unit   = output.append(s)
      override def normalFeedbackLn(s: String): Unit = { output.append(s); output.append('\n') }

      def reset(): Unit = { global.clear(); character.clear(); instruction.clear() }

      val paths = new collection.mutable.Stack[Path]

      private var notfoundCount = 0

      /**
       * What to do if the root bindings script file is not found.
       */
      def bindingsNotFound(path: Path): Unit = {
        if (notfoundCount==0) {
            profileWarning(s"""
           |USING THE BUILT-IN MINIMAL CONFIGURATION SCRIPT
           |
           |This is because there is no configuration script file: $path
           |
           |This is not catastrophic; but to avoid seeing this message again
           |
           |    1. ensure the ~/.red folder is present
           |    2. copy AppleRed.app/Contents/Resources/Bindings/* to ~/.red
           |
           |or provide a configuration file of your own and then restart the editor.
           |""".stripMargin)
          notfoundCount += 1
          minimalReadEvalPrint(minimalBindings, false, true)
        }
      }

      /**
       * RedScript minimal configuration. This provides functionality equivalent to
       * my original Red, but without abbreviations. It's here in case there's no
       * script in the expected location.
       */
      def minimalBindings = """
                              |persist  style   "Features" "Font Style"  "plain" (list "plain" "bold")
                              |persist  size    "Features" "Font Size"   18 (list 12 14 16 18 20 24 28)
                              |constant fontA (font (string "Monospaced" "/" style "/" size))
                              |constant fontB (font (string "Monospaced" "/" style "/" (- size 2)))
                              |constant fontC (font (string "Dialog" "/" "bold" "/" (max size 16)))
                              |UI:useFont fontA widget default button menu menubutton feedback
                              |UI:useFont fontB menu menubutton feedback
                              |UI:useFont fontC menu menubutton button
                              |constant shellCommands (list "wc" "ls -lt" "date" "printenv")
                              |
                              |(def (UI:pipeShellCommands path) shellCommands)
                              |(def (UI:needsLatex      path) (endsWith path ".tex"))
                              |(def (UI:latexBlockTypes path) latexblocktypes)
                              |
                              |
                              |(constant latexblocktypes
                              |  `(    foil     itemize   enumerate        -
                              |        note     exercise  answer           -
                              |        code     "-code"   "code*"  alltt   -
                              |        center   verbatim  comment  smaller -
                              |        question part      ans
                              |  )
                              |)
                              |
                              |(def (UI:unhandledInput key)
                              |     (seq (popup "Unhandled: " (inputToString key))
                              |          ()
                              |          ))
                              |
                              |(def (UI:pipeRedScripts path) ())
                              |(def (UI:needsPandoc path) (endsWith path ".md"))
                              |
                              |
                              |""".stripMargin

      def minimalReadEvalPrint(sourceString: String, show:  Boolean, throwError: Boolean): Unit =
          readEvalPrint(new Parser(io.Source.fromString(sourceString), "<Minimal Red Bindings>"), show, throwError)

      override def readEvalPrint(path: Path, show: Boolean, throwError: Boolean): Unit = {
          paths.push(path)
          try { super.readEvalPrint(path, show, false) }
          paths.pop()
      }

      def doInclude(args: List[SExp]): SExp = {
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

      def doPopup(message: List[SExp]): SExp = {
        val lines = message.map(_.toPlainString).mkString("", "\n", "")
        Dialog.showMessage(
          null,
          lines,
          "Bindings",
          Dialog.Message.Plain,
          icon = Utils.redIcon
        )
        Nothing
      }

      /** Declare mac diacriticals  */
      def doDia(strings: List[SExp]): SExp= {
        for { Str(mark) <- strings }  AltKeyboard.macKeyboardDiacritical = mark
        Nothing
      }

      /** Implements {{{(usefont font role1 role2 ...)}}} */
      def useFont(env: Env, params: SExp) : SExp = {
        val SExps(font :: roles) = params
        val roleNames = for {case Variable(role) <- roles} yield role
        font.eval(env) match {
          case FontExpr(fontName, font)  => Utils.setFontRoles(font, roleNames)
          case Str(fontName)             => Utils.setFontRoles(Utils.mkFont(fontName), roleNames)
        }
        Nothing
      }

      /** Implements {{{(persist name path menu-title initialValue choices)}}}*/
      def declPersistent(env: Env, params: SExp) : Const = {
        val SExps(Variable(name) :: args) = params
        val Str(path) :: Str(title) :: _value :: SExps(_choices) :: update = args.map(_.eval(env))
        val feature = new Persistent.StringFeature(name, path, _value.toPlainString, title) {
          override def choices: Seq[String] = _choices.map(_.toPlainString)
          override def toString(t: String): String = t
        }

        val persist = new LoadUpdate {
          override def toString: String = value.toString
          override def value: SExp = _value match {
            case _: Bool => Bool(feature.value.toBoolean)
            case _: Num => Num(feature.value.toInt)
            case _: Str => Str(feature.value)
          }

          override def setValue(newValue: SExp): Unit = (_value, newValue) match {
            case (_: Bool, Bool(v)) => feature.value=v.toString
            case (_: Num, Num(v)) => feature.value=v.toString
            case (_: Str, Str(v)) => feature.value=v
            case _          => throw RuntimeError(s"Persistent($name), initially ${_value}, cannot be set to $newValue")
          }
          override def eval(env: Env): SExp = value
        }
        global.set(name, persist)
        Features.add(feature)
        Nothing
      }

      /** Implements: {{{(tick name path menu-title initialValue)}}} */
      def declTick(env: Env, params: SExp) : Const = {
        val SExps(Variable(name) :: args) = params
        val Str(path) :: Str(title) :: Bool(value) :: update = args.map(_.eval(env))
        val feature = new Persistent.BoolFeature(name, path, value, title)
        val persist = new LoadUpdate {
          override def toString: String = value.toString
          override def value: SExp = Bool(feature.value)
          override def setValue(newValue: SExp): Unit = newValue match {
            case Bool(bool) => feature.value=bool
            case _          => throw RuntimeError(s"Tick($name) cannot be set to non-Bool $newValue")
          }
          override def eval(env: Env): SExp = value
        }
        global.set(name, persist)
        Features.add(feature)
        Nothing
      }

      val instruction: collection.mutable.HashMap[Instruction, SExp] = new collection.mutable.HashMap[Instruction, SExp]
      val character: collection.mutable.HashMap[Character, SExp] = new collection.mutable.HashMap[Character, SExp]

      /** The RedScript representation of an `EditSessionCommand` */
      case class EditSessionCommand(name: String, command: EditSessionCommands.SessionCommand) extends Const {
         override def toString: String = name
      }

      def declKeys(specs: List[SExp]) : Const = {
        if (logging) info(s"Keys Declared: $specs")
        for { Pair(Str(spec), effect) <- specs } Red.UserInput(spec) match  {
          case ch@Character(char, _, mods)    =>
            // key declarations take precedence over the AltKeyboard map
            if (ch.mods.hasAlt) AltKeyboard.mapTo(char.toUpper, char, mods.hasShift)
            character.addOne((ch, effect))
          case inst: Instruction  =>
            instruction.addOne((inst, effect))
          case other =>
            profileWarning(s"Declaring key $other")
        }
        if (logging) fine(s"CH: $character\nINST: $instruction")
        Nothing
      }

      def evalCommand(specs: List[SExp]) : Const =
        { val List(Str(name)) = specs
          CommandsDict(name) match {
            case None          => warn(s"(command $name) is not known"); Nothing
            case Some(command) => EditSessionCommand(name, command)
          }
        }

      def evalInsert(specs: List[SExp]) : Const =
      { val List(Str(chars)) = specs
        EditSessionCommand("insert", EditSessionCommands.insertCommand(chars))
      }

      def evalAndThen(specs: List[SExp]) : Const = {
        val command = specs.foldLeft(EditSessionCommand("", EditSessionCommands.doNothing)){
          case (EditSessionCommand(n1, c1), EditSessionCommand(n2, c2)) => EditSessionCommand(s"$n1; $n2", c1 &&& c2)
        }
        command
      }

      import Language._
      val bindingPrimitives: List[(String, Const)] = List(
        "abbrev"      -> Subr("abbrev",      {  case List(Str(abbr), Str(text)) => mapTo(abbr, text); Nothing }),
        "diacritical" -> Subr("diacritical", doDia(_)),
        "altclear"    -> Subr("altclear",    {  Nil => AltKeyboard.clear(); Nothing }),
        "include"     -> Subr("include",     doInclude(_)),
        // "module"     -> Subr("module",     doModule(_)), // TODO: (module name "path") defines a module environment from the file. module.name is a composite variable name
        "popup"       -> Subr("popup",       doPopup(_)),
        "UI:keys"     -> Subr("UI:keys",     declKeys(_)),
        "command"     -> Subr("command",     evalCommand(_)),
        "insert"      -> Subr("insert",      evalInsert(_)),
        "andThen"     -> Subr("andThen",     evalAndThen(_)),
        "hashCode"    -> Subr("hashCode",    { case List(value) => Num(value.hashCode) }),
        "inputToString" -> Subr("inputToString", { case List(UserInput(in)) => Str(in.toInput) }),
        "font"        -> Subr("font",        { case List(Str(name)) => FontExpr(name, Utils.mkFont(name))}),
        "UI:useFont"  -> FSubr("useFont",    useFont),
        "UI:cutringBound" -> Subr("UI:cutringBound", { case List(Num(bound)) => CutRing.bound = bound.toInt; Nothing; case Nil => Num(CutRing.bound)}),
        "persist"     -> FSubr("persist",    declPersistent),
        "tickbox"     -> FSubr("tickbox",    declTick),
        "readEval"    -> Subr  ("readEval", {
              case Str(text) :: Bool(show) :: rest =>
                   output.clear()
                   readEvalPrint(text, show, false)
                   val result = output.toString()
                   output.clear()
                   Str(result)
        }),
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

    /** Maps each included file to its timestamp */
    val fileTime = new collection.mutable.LinkedHashMap[File,Long]
    def anyFileChanged: Boolean = {
      fileTime.exists {
        case (file, time) => file.exists && file.lastModified()>time
      }
    }

    def importBindings(depth: Int, context: Path, path: Path, show: Boolean = false): Unit = {

    /**
     *  Read a single preferences file:
     *  @param depth the depth of include-nesting (with an ''ad-hoc'' limit to detect cyclic includes)
     *  @param context the path to the file being read
     */
      val file = path.toFile
      val timeStamp = if (file.exists) file.lastModified() else 0
      fileTime.addOne((file,timeStamp))

      def readFile(): Unit = {
        if (logging) info(s"importing bindings from: $file")
        RedScriptEvaluator.readEvalPrint(path, show, true)
      }

      if (file.exists()) {
         if (depth==0) {
           // re-read from root if necessary
           if (timeStamp>lastImportTime || anyFileChanged) {
             clearBindings()
             fileTime.clear()
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
           RedScriptEvaluator.bindingsNotFound(path)
        else
           throw AbortBindings(s"Attempting, from $context, to include:\n $path\nThere is no such bindings file\n")

    }

    case class AbortBindings(cause: String) extends Error(cause)

  }
}
