package RedLisp

object Test extends Global[Any] {
  import RedLisp.Syntax._
  type T = Any
  def v(s: String): SExp[Any] = Atom[Any](s)

  val List(a,b,c,d,e,f,g) = "abcdefg".map { c => v(s"$c") } . toList

  val ddef = Atom[T]("def")
  val ffun = Atom[T]("fun")
  val sset = Atom[T]("set")
  val iif = Atom[T]("if")
  val iifs = Atom[T]("if*")
  val one = Num[T](1)
  val two = Num[T](2)

  val tt = Bool[T](true)
  val ff = Bool[T](false)
  val ccons = v("cons")
  val llist = v("list")
  val isat = v("isatom")
  val isnum = v("isnum")
  val islist = v("islist")

  def run(sexps: SExp[T]*): Unit = {
    if (sexps.length==1) {
      print(s"${sexps.head} => ")
      println(s"${Run(sexps.head)}")
    } else {
      print(s"${sexps.mkString("(",  " ", ")")} => ")
      println(s"${Run(SExps(sexps.toList))}")
    }
  }

  def ss(sexps: SExp[T]*): SExp[T] = SExps(sexps.toList)
}
