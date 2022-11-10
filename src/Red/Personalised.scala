package Red

import RedScript.Language._
import RedScript.{Env, Evaluator, Language, SourcePosition}
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
  { applyScript("latexBlockTypes",  path) match {
      case SExps(es)  => es.map(_.toPlainString)
      case SExps(Nil) => Nil
      case other      => profileWarning(s"latexBlockTypes: path -> Seq[String]: $other"); Nil
    }
  }

  def needsLatex(path: String): Boolean = {
    Bindings.importBindings()
    applyScript("needsLatex", path) match {
      case Bool(bool) => bool
      case other      => profileWarning(s"needsLatex: path -> Bool: $other"); false
    }
  }

  def needsPandoc(path: String): Boolean = {
    Bindings.importBindings()
    applyScript("needsPandoc", path) match {
      case Bool(bool) => bool
      case other      => profileWarning(s"needsPandoc: path -> Bool: $other"); false
    }
  }

  def pipeShellCommands(path: String): Seq[String] =
  { Bindings.importBindings()
    applyScript("pipeShellCommands", path) match {
      case SExps(es) => es.map(_.toPlainString)
      case other     => profileWarning(s"pipeShellCommands: path -> Seq[String]: $other"); Nil
    }
  }

  def pipeRedScripts(path: String): Seq[String] =
  { Bindings.importBindings()
    applyScript("pipeRedScripts", path) match {
      case SExps(es) => es.map(_.toPlainString)
      case other     => profileWarning(s"pipeRedScripts: path -> Seq[fun]: $other"); Nil
    }
  }


  def clearBindings(): Unit = {
    Bindings.clearMapping()
    Bindings.RedScriptEvaluator.reset()

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
    var clickSelects:      Boolean = true
    var autoIndenting:     Boolean = true
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

      case class UserInput(input: Red.UserInput) extends Const
      case class SessionCommand(command: EditSessionCommands.SessionCommand) extends Const

      case class FontExpr(name: String, font: Font) extends Const {
        override def toString: String = s"(font \"$name\")"
      }

      val output = new StringBuilder()

      override def errorFeedback(s: String): Unit = throw AbortBindings(s)
      override def normalFeedback(s: String): Unit = output.append(s)
      override def normalFeedbackLn(s: String): Unit = output.append(s)

      def reset(): Unit = { global.clear(); character.clear(); instruction.clear() }

      val paths = new collection.mutable.Stack[Path]

      override def readEvalPrint(path: Path, show: Boolean): Unit = {
          paths.push(path)
          super.readEvalPrint(path, show)
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

      def doAlt(shifted: Boolean)(args: List[SExp]): SExp = {
        for { Str(map) <- args }  AltKeyboard.mapTo(map(0).toUpper, map(1), shifted)
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

      def declKey(specs: List[SExp]) : Const = {
        if (logging) info(s"Keys Declared: $specs")
        for { Pair(Str(spec), effect) <- specs } Red.UserInput(spec) match  {
          case ch:   Character    => character.addOne((ch, effect))
          case inst: Instruction  => instruction.addOne((inst, effect))
          case other => profileWarning(s"Declaring key $other")
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
        "altplain"    -> Subr("altplain",    doAlt(false)(_)),
        "altshift"    -> Subr("altshift",    doAlt(true)(_)),
        "include"     -> Subr("include",     doInclude(_)),
        // "module"     -> Subr("module",     doModule(_)), // TODO: (module name "path") defines a module environment from the file. module.name is a composite variable name
        "popup"       -> Subr("popup",       doPopup(_)),
        "keys"        -> Subr("keys",        declKey(_)),
        "keySpec"     -> Subr("keySpec",     { case List(Str(spec)) => Str(s"$spec ==> ${Red.UserInput(spec).toString}")}),
        "modSpec"     -> Subr("modSpec",     { case  List(Str(spec)) => UserInputDetail.Detail.withDetail(spec) match {
                                                                          case None => nil
                                                                          case Some(detail) => Num(detail.mods)
                                                                        }}),
        "command"     -> Subr("command",     evalCommand(_)),
        "andThen"     -> Subr("andThen",     evalAndThen(_)),
        "hashCode"    -> Subr("hashCode",    { case List(value) => Num(value.hashCode) }),
        "inputToString" -> Subr("inputToString", { case List(UserInput(in)) => Str(in.toInput) }),
        "font"        -> Subr("font",        { case List(Str(name)) => FontExpr(name, Utils.mkFont(name))}),
        "useFont"     -> FSubr("useFont",    useFont),
        "persist"     -> FSubr("persist",    declPersistent),
        "tickbox"     -> FSubr("tickbox",    declTick),
        "readEval"    -> Subr  ("readEval", {
              case Str(text) :: Bool(show) :: rest =>
                   output.clear()
                   readEvalPrint(text, show)
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
        RedScriptEvaluator.readEvalPrint(path, show)
      }


      if (file.exists()) {
         if (depth==0) {
           // re-read root if necessary
           /*
           if (timeStamp>lastImportTime) {
              clearBindings()
              readFile()
              lastImportTime = timeStamp
           }
           */
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
           throw AbortBindings(s"No bindings file $path   (this is not catastrophic)\nEither copy AppleRed.app/Contents/Resources/Bindings to ~/.red\nor export REDBINDINGS=...a bindings file...")
        else
           throw AbortBindings(s"Attempting, from $context, to include:\n $path\nThere is no such bindings file\n")

    }

    case class AbortBindings(cause: String) extends Error(cause)

  }
}
