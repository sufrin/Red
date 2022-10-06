package Red

import Red.Features.Feature
import Useful.PrefixMap

import java.io.FileInputStream
import java.nio.file.{Path, Paths}
import java.util.NoSuchElementException
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
  def latexBlockTypes: Seq[String] =
  { val default = "foil itemize enumerate - note exercise answer - code -code code* scala alltt - center verbatim comment smaller - question part ans"
    Bindings.importBindings()
    if (personalBlockTypes.isEmpty)
       FilterUtilities.parseArguments(sys.env.getOrElse("REDLATEXBLOCKS", default))
    else
    personalBlockTypes.toList
  }

  /** Program names to be placed on the `Pipe` menu */
  val personalPipeNames = new collection.mutable.ListBuffer[String]
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
  }

  def warning(heading: String, message: String): Unit = {
    Dialog.showMessage(
      null,
      s"$message${if (heading.nonEmpty) s"\n$heading" else ""}",
      "Bindings",
      Dialog.Message.Warning,
      icon = Utils.redIcon
    )
  }

  def profileWarning(message: String): Unit = warning(s"Profile: ${Features.profile}", message)

  object Settings {
    var typeOverSelection: Boolean = true
    var clickSelects: Boolean = true
    var autoIndenting: Boolean = true
  }

  object Bindings {
    val feedback: Notifier[String] = new Notifier[String]("Personalised Feedback")

    val profileChanged: Notifier[String] = Features.profileChanged

    def profile: String = Features.profile

    private val trie = PrefixMap[String]()
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

    /** Modification time of the last root bindings file */
    var lastImportTime: Long = 0

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

    case class AbortBindings(cause: String) extends Error(cause)

    /** Read the top-level bindings file if it has been updated since it was last read */
    def importBindings(): Unit = {
      val path = sys.env.getOrElse("REDBINDINGS", "~/.red/red.bindings")
      val top = Paths.get("")
      try importBindings(profile, 0, top, toPath(top, path)) catch {
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
    /**
     *  Read a single preferences file:
     *  @param profile the profile being matched
     *  @param depth the depth of include-nesting (with an ''ad-hoc'' limit to detect cyclic includes)
     *  @param context the path to the file being read
     */
    def importBindings(profile: String, depth: Int, context: Path, path: Path): Unit = {
      val file = path.toFile
      val timeStamp = if (file.exists) file.lastModified() else 0
      def readFile(): Unit = {
        import scala.io.BufferedSource
        if (logging) info(s"importing bindings from: $file")
        val source = new BufferedSource(new FileInputStream(file))
        val thisContext = context.resolve(path)
        var lineNumber = 1
        for { line <- source.getLines() } {
          processLine(profile, depth, thisContext, lineNumber, stripComments(line))
          lineNumber += 1
        }
        source.close()
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


    object Lexical {
      import sufrin.regex.Regex
      import sufrin.regex.syntax.{Branched, Parser}
      /** Build a lexer (a `Branched` pattern) for symbols matching the given regex patterns. */
      def build(pats: String*): Regex =
      { val trees = pats.map(pat => new Parser(pat).tree)
        new Regex(Branched(trees), false, false)
      }
      /** Lexemes are:
       *
       *     0. sequences of space
       *
       *     1-2. non-quoted sequences of non-space (including unicode character descriptions
       *     of the form \uXXXX)
       *
       *     3. quoted sequences of non-quote (including unicode character descriptions)
       */
      val lexer = build("""\s*""", """([^ "]\S+)""", """((\\[Uu]\w\w\w\w|[^ "])+)""", """\"((?:\\[Uu]\w\w\w\w|[^"])+)\"""")
      /**
       *   Scan the line, yielding a list of the non-space fragments of text, with
       *   unicode-character descriptions transformed into characters.
       */
      def scan(line: String): List[String] = {
        val it = for {sym <- lexer allPrefixes line if sym.theMatch.index!=0} yield {
          if (logging) finest(s"raw: $sym")
             sym.theMatch.index match {
               case 1 | 2 | 3 => expandEscapes(sym.group(1)) // expand unicodes codes, etc
             }
        }
        it.toList
      }
    }

    /**
     *  Process a single line of the preferences file:
     *  @param profile the profile being matched
     *  @param depth the depth of include-nesting (with an ''ad-hoc'' limit to detect cyclic includes)
     *  @param context the path to the file being read
     *  @param lineNumber  the number of the line in the file being read
     *
     */
    def processLine(profile: String, depth: Int, context: Path, lineNumber: Int, line: String): Unit = if (line.nonEmpty) {


      def ifSo(b: Boolean): Boolean  = b
      def ifNot(b: Boolean): Boolean = !b

      case object Undefined extends Feature {
        val name: String = "Undefined"
        def valueString: String = "Undefined"
        def profileString: String = "Undefined"
        def processConditional(tail: List[String], process: List[String] => Unit, ifSo: Boolean => Boolean): Unit = {
          throw new AbortBindings(s"Erroneous conditional declaration (undeclared feature):\n$line\n($context@$lineNumber)")
        }
        def save(): Unit = {}
        def restore(): Unit = {}
      }

      def evalDeclaration(declaration: List[String]): Unit = declaration match {

        case "--" :: _ =>

        case "??" :: rest =>
          for { (_, feature) <- Features.features}  println(s"$feature = ${feature.valueString}")
          for { fid <- rest } println(s"$fid = ${Features.eval(fid) }")

        case ("if"     :: feature :: restOfDeclaration)       =>  Features.getOrElse(feature, Undefined).processConditional(restOfDeclaration, evalDeclaration, ifSo)
        case ("&&"     :: feature :: restOfDeclaration)       =>  Features.getOrElse(feature, Undefined).processConditional(restOfDeclaration, evalDeclaration, ifSo)
        case ("then"   :: feature :: restOfDeclaration)       =>  Features.getOrElse(feature, Undefined).processConditional(restOfDeclaration, evalDeclaration, ifSo)
        case ("unless" :: feature :: restOfDeclaration)       =>  Features.getOrElse(feature, Undefined).processConditional(restOfDeclaration, evalDeclaration, ifNot)

        case ("font" :: kind :: style :: size :: roles) =>
          Utils.setFont(kind, Features.eval(style), Features.eval(size), roles)

        case ("feature" :: name :: kind :: attrs) =>
             Features.declare(name, kind, attrs) match {
               case Some(error) => profileWarning(declaration.mkString("Erroneous declaration:\n", " ", s"\n($context@$lineNumber)\n$error"))
               case None =>
             }

        case ("include" :: path :: Nil)  =>
          val exPath = toPath(context, path)
          if (exPath.toFile.exists() && exPath.toFile.canRead())
             importBindings(profile, depth+1, context, exPath)
          else
            profileWarning(s"Attempting, from $context@$lineNumber, to include:\n$exPath\nNo such bindings file can be read.")

        case ("include?" :: path :: Nil) =>
          val exPath = toPath(context, path)
          if (exPath.toFile.exists()  && exPath.toFile.canRead())
            importBindings(profile, depth+1, context, exPath)

        case ("text"::"abbrev"::abbr::text::_)  =>
          mapTo(abbr, text)

        case ("pipes"::abbrevs) =>
          personalPipeNames.addAll(abbrevs)

        case ("latex"::"blocks"::abbrevs) =>
          personalBlockTypes.addAll(abbrevs)

        case ("show" :: fields) =>
          println(fields.map {
                 case "$profile" => Features.profile
                 case other      => Features.eval(other)
              }.mkString("", " ", ""))

        case ("text"::"diacritical" :: marks :: rest) =>
          AltKeyboard.macKeyboardDiacritical = marks

        case ("text" :: "alt" :: "reset" :: rest) =>
          AltKeyboard.clear()

        case ("text" :: "alt" :: "shift" :: pairs ) =>
          for { map <- pairs if map.length>=2 }
            AltKeyboard.mapTo(map(0).toUpper, map(1), shift=true)

        case ("text" :: "alt" :: pairs ) =>
          for { map <- pairs if map.length>=2 }
            AltKeyboard.mapTo(map(0).toUpper, map(1), shift=false)

        case (variable :: "is" :: _value :: Nil) =>
             try {
               val value = Features.eval(_value)
               variable match {
                 case "typeover"        => Settings.typeOverSelection = value.toBoolean
                 case "autoselect"      => Settings.clickSelects = value.toBoolean
                 case "autoindent"      => Settings.autoIndenting = value.toBoolean
                 case "matchcostlimit"  => Utils.stepLimit = value.toInt
                 case "showmatchcost"   => Utils.showSteps = value.toBoolean
               }
             } catch {
               case exn: Throwable => warning(profile, declaration.mkString("Erroneous variable setting:\n", " ", s"\n($context@$lineNumber)"))
             }

        case other =>
          profileWarning(other.mkString("Erroneous binding declaration:\n", " ", s"\n($context@$lineNumber)"))
      }
      try evalDeclaration(Lexical.scan(line)) catch {
        case exn: NoSuchElementException => profileWarning(s"Conditional declaration with undefined feature: ${exn.getMessage}\n($context@$lineNumber)")
      }
    }

    def stripComments(line: String): String = {
      val b = new StringBuilder()
      var l = 0
      var r = line.length()
      // find the extent of the space-trimmed line
      while (0 < r && (line.charAt(r - 1) == ' ' || line.charAt(r - 1) == '\t')) r -= 1
      while (l < r && (line.charAt(l) == ' ' || line.charAt(l) == '\t')) l += 1
      // strip trailing comments, introduced by unquoted '#'
      while (l < r && line.charAt(l)!='#') {
        val c = line.charAt(l)
        if (c == '\\' && l + 1 < r && line.charAt(l + 1) == '#') {
          b.append("\\#")
          l += 1
        }
        else
          b.append(c)
        l += 1
      }
      b.toString()
    }

    def expandEscapes(str: String): String = {
      val b = new StringBuilder
      var i = 0
      val l = str.length
      while (i<l) {
        if (str.charAt(i) == '\\' && i + 1 < l)
          str.charAt(i + 1) match  {
            case '\\' =>  b append "\\"
            i += 1

            case '#' =>  b append "#"
            i += 1

            case 't' =>  b append  "\t"
            i += 1

            case 'n' =>  b append "\n"
            i += 1;

            case ' ' | 's' =>  b append " "
            i += 1;

            case 'u' | 'U' =>
            if (i + 5 < l) {
              import Useful.CharSequenceOperations._
              str.subSequence(i, i + 6).toUnicode match {
                case Some(ch) =>
                  b append ch
                  i += 5
                case None =>
                  b append '\\'
              }
            }

            case ch =>  b append '\\'; b append ch
              i += 1
          }
        else
          b.append(str.charAt(i))
        i += 1
      }
      b.toString
    }


  }
}
