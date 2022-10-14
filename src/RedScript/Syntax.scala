package RedScript



object Syntax {

  trait SExp {
    def eval(env: Env): Const

    def lval(env: Env): Ref = throw RuntimeError(s"$this cannot be assigned to $position")

    def isNull: Boolean = false

    def evalQuote(env: Env): Const = eval(env)

    var position: SourcePosition = new SourcePosition(("",-1,-1))

  }


  case class Atom(name: String) extends SExp {
    def eval(env: Env): Const = env(name) match {
      case None => throw RuntimeError(s"Unbound variable $name ($position)")
      case Some(v) => v match {
        case Ref(value) => value
        case _ => v
      }
    }

    override def lval(env: Env): Ref = env(name) match {
      case None => throw RuntimeError(s"Unbound variable $name ($position)")
        case Some(v)   => v match {
        case r: Ref    => r
        case _         => throw RuntimeError(s"Not an lvalue $name ($position)")
      }
    }

    override def toString = name
  }

  case class Ref(var value: Const) extends Const {
    override def eval(env: Env): Const = value
    override def lval(env: Env): Ref = this
  }

  case class Seq(elements: List[Const]) extends Const {
    override def toString = elements.mkString("(", " ", ")")
    override def evalQuote(env: Env): Const = SExps(elements.map{ case Quote(e) => e; case e => e}).eval(env)
  }

  case class SExps(elements: List[SExp]) extends SExp {
    override def toString = elements.mkString("(", " ", ")")

    override def isNull = elements.isEmpty

    def withErrorHandling(value: => Const): Const = {
        try value catch  {
          case exn: RuntimeError => throw RuntimeError(s"${exn.getMessage} in $this $position")
          case exn: SyntaxError => throw SyntaxError(s"${exn.getMessage} in $this $position")
        }
    }


    def eval(env0: Env): Const =
      if (elements.isEmpty) Seq(Nil) else {
          val operator = elements.head.eval(env0)
          val result =
            operator match {
              case Subr(_, scala) =>
                withErrorHandling {
                  val args = elements.tail.map(_.eval(env0))
                  scala(args)
                }

              case FSubr(_, scala) =>
                withErrorHandling {
                  val args = SExps(elements.tail)
                  scala(env0, args)
                }

              case Expr(env1, params, body) =>
                withErrorHandling {
                  val args = elements.tail.map(_.eval(env0))
                  body.eval(env1.extend(params, args))
                }

              case other =>
                throw RuntimeError(s"$operator is not a functional value: $this ($position)")
            }
            result
      }
  }


  /**
   * The fixedpoints of `eval` are represented as `Const`s.
   */
  trait Const extends SExp {
    def eval(env: Env): Const = this
  }

  case class Num(value: Int) extends Const {
    override def toString = value.toString
  }


  case class Bool(value: Boolean) extends Const {
    override def toString = value.toString
  }

  case class Str(value: String) extends Const {
    override def toString = s"\"$value\""
  }

  case class Expr(env: Env, pattern: SExp, body: SExp) extends Const {
    override def toString = s"(expr $pattern $body)"
  }

  case class Subr(name: String, scala: List[Const] => Const) extends Const {
    override def toString = s"Strict: $name"
  }

  case class FSubr(name: String, scala: (Env, SExp) => Const) extends Const {
    override def toString = s"Lazy: $name"
  }

  case class Opaque(value: Any) extends Const {
    override def toString = s"Opaque: $value"
  }

  case class Quote(value: SExp) extends Const {
    override def toString = s"'$value"
    override def evalQuote(env: Env): Const = value.evalQuote(env)
  }

  case class RuntimeError(why: String, description: Option[Any] = None, level: Int=0) extends scala.Error(why) with Const {
    override def eval(env: Env): Const = throw this
    override def toString: String = s"Runtime Error: $why ${if (description.isEmpty) "" else description}"
    def reThrow(): Const =
      if (level<4) throw new RuntimeError(why, description, level+1) else this
  }

  case class SyntaxError(why: String, description: Option[Any] = None) extends scala.Error(why) with Const {
    override def eval(env: Env): Const = throw this
    override def toString: String = s"Syntax Error: $why ${if (description.isEmpty) "" else description}"
  }

}
