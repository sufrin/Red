import Useful.Format
import Useful.Format._


trait Expr {
  def fmt: Format
  def apply(n: Int): Unit = println(this.fmt.asString(n))
}

case class S(seq: Seq[Expr]) extends Expr {
  def fmt: Format =
    { val body = seq.map(_.fmt).reduce((l,r)=>l:/:r)
      :::("[", indent(1)(body), "]")
      //:::("begin ", optIndent(6)(body), (), "end")
    }
}

case class A(atom: Any) extends Expr {
  def fmt: Format = :::(atom)
}

/** Construct an S-expression */
def s(es: Any*): Expr =
  es.map{
     case e: Expr => e
     case s: String => A(s)
     case n: Int => A(n)
  } match {
    case List()    => A("nil")
    case h::List() => h
    case ts        => S(ts)
  }

val a = s("foo", "baz", "foobaz")
a(80)
a(5)
a(0)

val b = s(a,"xyzzy", a)
b(180)
b(20)

val c = s(a, b, "middle of the road", b, a)
c(100)
c(20)
c(10)





