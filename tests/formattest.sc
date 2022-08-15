

/*
val f = Format.|||(
  "the", (), (4, List("rain",(), "in", (), "spain")), List("fallse", "mainly")
)
*/
import Useful.Format.{empty, nest, |||, ind}
val f = nest(5) ("the" :/: "rain" :/: "in" :/: "spain" :/: empty)
f.asString(32)
f.asString(8)
f.asString(16)


val g = |||("the", (), "rain", (), "in", (), "spain")
g.asString(8)


val h = |||("the", (), nest(4) (|||("rain", (), nest(4)(|||("in",(), "my")), (), "spain")), (), "falls", (), "mainly")
h.asString(10)
h.asString(8)
h.toString

val k = |||("able", (), ind(5)(|||("f", (), "g", (), "h")), (), "baker")
k.asString(60)
k.asString(8)






