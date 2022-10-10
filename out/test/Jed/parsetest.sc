import RedScript._

import scala.io.Source

val s1 =
  """
    |true
    |false
    |( if true false true)
    |   # comments
    |   # comments
    |   (' farly)
    |   -->Oxymoron<--
    |   234
    |   "foobaz"
    |   456
    |""".stripMargin

val p2 = new Parser(Source.fromString(s1))
while (p2.nextSymb()!=Lexical.EOF) { print(s"${p2.position}: "); println(p2.read) }

val p1 = new Parser(Source.fromString(s1))
while (p1.hasNextSymb) { print(s"$p1: "); println(p1.nextSymb()) }


val s2 =
  """
    |23
    |24
    |(if true 23 24)
    |(def fst(x y) x)
    |(def snd(x y) y)
    |(fst 3 4)
    |(snd 3 4)
    |list 1 2 3 (fst 4 5) (list 6 "seven" 7 8)
    |""".stripMargin

Test.rep(s2)





