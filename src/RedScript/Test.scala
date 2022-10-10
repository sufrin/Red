package RedScript

import scala.io.Source

object Test extends Runtime {

  def rep(source: String): Unit = {
    val p = new Parser(Source.fromString(source))
    while (p.nextSymb()!=Lexical.EOF)  {
         val e = p.read
         val r = try Run(e).toString catch { case exn: Exception => exn.toString }
         println(s"$e => $r")
    }
  }
  def rp(source: String): Unit = {
    val p = new Parser(Source.fromString(source))
    while (p.nextSymb()!=Lexical.EOF)  {
      val e = p.read
      println(s"$e => ")
    }
  }
}
