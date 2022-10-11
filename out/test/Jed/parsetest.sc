import RedLisp._

import scala.io.Source

val p = new Parser(Source.fromString("(able baker 347 ->Oxymoron )"))

for { i<-0 until 20 } println(p.nextSymb())


