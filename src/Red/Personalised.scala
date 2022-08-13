package Red

import Useful.PrefixMap

/**
 *  Personalisation module, with definitions of
 *  abbreviations, menu entries, etc.
 *
 *  TODO: read configuration data to set this up.
 *
 */
object Personalised {
  /** Block types to be placed on a `Latex` menu */
  def latexBlockTypes: Seq[String] =
  { val default = "foil itemize enumerate - note exercise answer - code -code code* scala alltt - center verbatim comment smaller - question part ans"
    val blocks = Jed.FilterUtilities.parseArguments(sys.env.getOrElse("REDLATEXBLOCKS", default))
    blocks
  }

  /** Program names to be placed on the `Pipe` menu */
  def pipeNames: Seq[String] =
  { val default = "wc; ls -lt; printenv"
    val names = sys.env.getOrElse("REDPROGRAMS", default).split(";[ ]+").toList
    names
  }

  object Abbrev {
    val trie = PrefixMap[String]()
    def add(abbrevs: (String, String)*): Unit =
        for { (abbrev, result) <- abbrevs } trie.addOne(abbrev.reverse, result)

    add (
      "=>"  -> "\\Rightarrow"
    , "==>" -> "\\Longrightarrow"
    )
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
