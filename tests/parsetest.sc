import RedLisp._

import scala.io.Source

val p = new Parser(Source.fromString("able =>baker \n #foobaz os best\n (347 ->Oxymoron )?"))

while (p.hasNextSymb) println(p.nextSymb())


