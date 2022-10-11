package RedLisp

object Syntax {

  trait SExp[T] {
    def eval(env: Env[T]): Const[T]

    def isNull: Boolean = false
  }

  case class Atom[T](name: String) extends SExp[T] {
    def eval(env: Env[T]): Const[T] = env(name)

    override def toString = name
  }

  case class Seq[T](elements: List[Const[T]]) extends Const[T] {
    override def toString = elements.mkString("(", " ", ")")
  }

  case class SExps[T](elements: List[SExp[T]]) extends SExp[T] {
    override def toString = elements.mkString("(", " ", ")")

    override def isNull = elements.isEmpty

    def eval(env: Env[T]): Const[T] =
      if (elements.isEmpty) Seq(Nil) else {
        val operator = elements.head.eval(env)
        operator match {
          case Subr(_, scala) =>
            val args = elements.tail.map(_.eval(env))
            scala(args)

          case FSubr(_, scala) =>
            val args = SExps(elements.tail)
            scala(env, args)

          case Expr(env, params, body) =>
            val args = elements.tail.map(_.eval(env))
            body.eval(env.extend(params, args))

          case other =>
            throw Error(s"$operator is not applicable", Some(this))
        }
      }
  }


  /**
   * The fixedpoints of `eval` are represented as `Const`s.
   */
  trait Const[T] extends SExp[T] {
    def eval(env: Env[T]): Const[T] = this
  }

  case class Num[T](value: Int) extends Const[T] {
    override def toString = value.toString
  }


  case class Bool[T](value: Boolean) extends Const[T] {
    override def toString = value.toString
  }

  case class Str[T](value: String) extends Const[T] {
    override def toString = s"\"$value\""
  }

  case class Expr[T](env: Env[T], pattern: SExp[T], body: SExp[T]) extends Const[T] {
    override def toString = s"(expr $pattern $body)"
  }

  case class Subr[T](name: String, scala: List[Const[T]] => Const[T]) extends Const[T] {
    override def toString = s"Strict: $name"
  }

  case class FSubr[T](name: String, scala: (Env[T], SExp[T]) => Const[T]) extends Const[T] {
    override def toString = s"Lazy: $name"
  }

  case class Opaque[T](value: T) extends Const[T] {
    override def toString = s"Opaque: $value"
  }

  case class Quote[T](value: SExp[T]) extends Const[T] {
    override def toString = s"(' $value)"
  }

  case class Error[T](why: String, description: Option[Any] = None) extends scala.Error(why) with Const[T] {
    override def eval(env: Env[T]): Const[T] = throw this

    override def toString: String = s"Error($why, $description)"
  }

}
