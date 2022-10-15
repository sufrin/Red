package RedScript

import scala.io.Source
import RedScript.Language._

object Test extends Evaluator {

  def rep(source: String): Unit = {
    val p = new Parser(Source.fromString(source))
    try {
      while (p.nextSymb() != Lexical.EOF) try {
        val e = p.read
        val r = try Run(e).toString catch {
          case exn: RuntimeError => exn
        }
        println(s"$e => $r")
      } catch {
        case exn: SyntaxError =>  println(exn)
      }
    }
    catch {
      case exn: SyntaxError =>  println(exn)
      case exn: Exception   => println(exn)
    }
  }
  def rp(source: String): Unit = {
    val p = new Parser(Source.fromString(source))
    while (p.nextSymb()!=Lexical.EOF) {
      try {
      val e = p.read
      println(s"${e.position}: $e => ")
    } catch {
      case exn: SyntaxError =>  println(s"${p.position}: $exn")}
    }
  }
}
