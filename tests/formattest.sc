import Useful.Format
import Useful.Format.{:::, empty, ind, nest, group}

import java.io.Writer

trait Expr {
  def fmt: Format
  def apply(n: Int): Unit = println(this.fmt.asString(n))
}
case class S(seq: Expr*) extends Expr {
  def fmt: Format =
    { val body = seq.map(_.fmt).reduce((l,r)=>l:/:r)
      :::("[{", nest(2)(body), (), "}]")
    }
}
case class A(atom: Any) extends Expr {
  def fmt: Format = :::(atom)
}

val a = S(A("foo"), A("baz"), A("foobaz"))
a(20)
a(5)
val b = S(a,A("xyzzy"), a)
b(60)
b(20)


val f = nest(5) ("the" :/: "rain" :/: "in" :/: "spain" :/: empty)
f.asString(32)
f.asString(8)
f.asString(16)

val g = :::("the", (), "rain", (), "in", (), "spain")
g.asString(8)


val h = :::("the", (), nest(4) (:::("rain", (), nest(4)(:::("in",(), "my")), (), "spain")), (), "falls", (), "mainly")
h.asString(10)
h.asString(8)
h.toString

val k = :::("able", (), ind(5)(:::("f", (), "g", (), "h")), (), "baker")
k.asString(60)
k.asString(8)






