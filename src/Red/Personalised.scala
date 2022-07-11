package Red

object Personalised {
  /** Block types to be placed on a `Latex` menu */
  def latexBlockTypes: Seq[String] =
  { val default = "foil itemize enumerate - note exercise answer - code -code code* scala alltt - center verbatim comment smaller - question part ans"
    val blocks = Jed.FilterUtilities.parseArguments(sys.env.getOrElse("REDLATEXBLOCKS", default))
    blocks
  }

  /** Program names to be placed on the `Pipe` menu */
  def pipeNames: Seq[String] =
  { val default = "wc; ls -lt"
    val names = sys.env.getOrElse("REDPROGRAMS", default).split(";[ ]+").toList
    names
  }


}
