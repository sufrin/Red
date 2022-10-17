package RedScript

import RedScript.Language._

object Test extends Evaluator {

  def rp(source: String): Unit = {
    val parser = new Parser(io.Source.fromString(source))
    parser.syntaxEnv=syntaxEnv
    while (parser.nextSymb() != Lexical.EOF) {
      try {
        val e = parser.read
        println(s"${e.position}: $e => ")
      } catch {
        case exn: SyntaxError => println(s"${parser.position}: $exn")
      }
    }
  }

}
