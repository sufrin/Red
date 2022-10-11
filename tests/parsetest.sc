import RedLisp._

import scala.io.Source

val p1 = new Parser(Source.fromString("able =>baker \n #foobaz os best\n (347 ->Oxymoron )?"))

while (p1.hasNextSymb) println(p1.nextSymb())

val p2 = new Parser[Any](Source.fromString("( (if true false true)  (able =>baker \n #foobaz os best\n (347 ->Oxymoron ))"))

p2.nextSymb()
p2.expr[Any]
p2.symb




