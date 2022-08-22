package Red

import Useful.PrefixMap

import java.io.FileInputStream
import java.nio.file.{Path, Paths}

/**
 *  Personalisation module, with definitions of
 *  abbreviations, menu entries, etc.
 *
 *  TODO: read configuration data to set this up.
 *
 */
object Personalised extends Logging.Loggable {

  /** Block types to be placed on a `Latex` menu */
  val personalBlockTypes =  new collection.mutable.ListBuffer[String]
  def latexBlockTypes: Seq[String] =
  { val default = "foil itemize enumerate - note exercise answer - code -code code* scala alltt - center verbatim comment smaller - question part ans"
    Bindings.importBindings()
    if (personalBlockTypes.isEmpty)
       Jed.FilterUtilities.parseArguments(sys.env.getOrElse("REDLATEXBLOCKS", default))
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
  }

  object Bindings {
    val warning: Notifier[String] = new Notifier[String]("Personalized Warning")
    val feedback: Notifier[String] = new Notifier[String]("Personalized Feedback")

    private val trie = PrefixMap[String]()
    def longestSuffixMatch(chars: CharSequence, upTo: Int): Option[(String, Int)] = {
       importBindings()
       trie.longestSuffixMatch(chars, upTo)
    }

    /** Add an abbreviation mapping */
    def addMap(abbrevs: (String, String)*): Unit =
        for { (abbrev, result) <- abbrevs } mapTo(abbrev, result)

    def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(abbrev, result)

    /** Add a single cycle of abbreviatins */
    def addCycle(abbrevs: String*): Unit = {
      for { i<-0 until abbrevs.length -1 } trie.reverseUpdate(abbrevs(i), abbrevs(i+1))
    }

    /** Modification time of the last root bindings file */
    var lastImportTime: Long = 0

    def toPath(context: Path, path: String): Path =
      if (path.startsWith("~/"))
        java.nio.file.Paths.get(Jed.Utils.expandHome(path))
      else {
        val thePath = java.nio.file.Paths.get(path)
        if (thePath.isAbsolute)
          thePath
        else
          context.resolveSibling(thePath)
      }

    case class AbortBindings(cause: String) extends Error(cause)

    /** Read the top-level bindings file, if it has been updated since it was last read */
    def importBindings(): Unit = {
      val path = sys.env.getOrElse("REDBINDINGS", "~/.red/red.bindings")
      val top = Paths.get("")
      try importBindings(0, top, toPath(top, path)) catch {
        case AbortBindings(why) => warning.notify(why)
      }
    }

    /** Read the top-level bindings file (unconditionally) */
    def reImportBindings(): Unit = {
      lastImportTime = 0
      importBindings()
    }

    def importBindings(depth: Int, context: Path, path: Path): Unit = {
      val file = path.toFile
      val timeStamp = if (file.exists) file.lastModified() else 0
      def readFile(): Unit = {
        import scala.io.BufferedSource
        if (logging) info(s"importing bindings from: $file")
        val source = new BufferedSource(new FileInputStream(file))
        val thisContext = context.resolve(path)
        var lineNumber = 1
        for { line <- source.getLines() } {
          processLine(depth, thisContext, lineNumber, stripComments(line))
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
         if (depth<6)
           readFile()
         else
           throw AbortBindings(s"Including $path\nfrom $context\nthis bindings file forms a cycle or is nested too deeply")
      } else
           throw AbortBindings(s"Including $path\nfrom $context\nthere is no such bindings file")
    }

    object Lexical {
      import sufrin.regex.Regex
      import sufrin.regex.syntax.{Branched, Parser}
      def build(pats: String*): Regex =
      { val trees = pats.map(pat => new Parser(pat).tree)
        new Regex(Branched(trees), false, false)
      }
      val pat = build("""\s*""", """([^ "]\S+)""", """((\\[Uu]\w\w\w\w|[^ "])+)""", """\"((?:\\[Uu]\w\w\w\w|[^"])+)\"""")
      def scan(line: String): List[String] = {
        val it = for { sym <- pat allPrefixes line if sym.theMatch.index!=0 } yield {
          if (logging) finest(s"raw: $sym")
             sym.theMatch.index match {
               case 1 | 2 | 3 => expandEscapes(sym.group(1)) // expand unicodes codes, etc
             }
        }
        it.toList
      }
    }

    /** Context is the path to the file being read; depth is the depth of include-nesting */
    def processLine(depth: Int, context: Path, lineNumber: Int, line: String): Unit = if (line.nonEmpty) {
      val fields: List[String] = Lexical.scan(line) // line.split("\\s+").toList
      if (logging) finer(s"Binding $fields")
      fields match {
        case ("include" :: path :: _)  => importBindings(depth+1, context, toPath(context, path))
        case ("include?" :: path :: _) =>
          val exPath = toPath(context, path)
          if (exPath.toFile.exists())
            importBindings(depth+1, context, exPath)
        case ("text"::"abbrev"::abbr::text::_)  =>
          mapTo(abbr, text)
        case ("pipes"::abbrevs) =>
          personalPipeNames.addAll(abbrevs)
        case ("latex"::"blocks"::abbrevs) =>
          personalBlockTypes.addAll(abbrevs)
        case ("show"::fields) =>
          feedback.notify(fields.mkString("", " ", ""))
        case ("text"::"diacritical" :: marks :: rest) =>
          Red.InputPanel.macKeyboardDiacritical = marks
        case other =>
          warning.notify(other.mkString("Erroneous binding declaration:\n", " ", s"\n($context@$lineNumber)"))
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

  /*
  locally {
    import java.util.prefs.Preferences
    val
    val root = Preferences.userRoot()

    val redPrefs = root.node("red")
    val latexPrefs = root.node("red/latex")
    val pipePrefs = root.node("red/pipe")
    pipePrefs.put("wc", "")
    pipePrefs.put("ls", "-lt")
    for {block <- latexBlockTypes} latexPrefs.put(block, "")
    val dump = new FileOutputStream("./.RedPrefs.xml")
    redPrefs.exportSubtree(dump)
    dump.close()
  }
  */
}
