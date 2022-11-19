package RedScript

/**
 * Abstract syntax and "pre-semantics" of S-expressions.
 *
 * Each `SExp` form has an interpretation in an environment: either as an lvalue
 * (address of a constant: recovered by `lval(env)`)
 * or as a constant (recovered by `eval(env)`).
 *
 * Constants are also `SExp` forms: their interpretations, save for those of
 * variables, are in normal form -- that is, they denote themselves.
 *
 * The interpretation of a composite S-expression of the form
 * `(`''operator'' ''operands''`)` is very flexible: and depends on
 * the interpretation of the ''operator''. The details are specified in
 * the `Evaluator` module; and a number of detailed examples should be
 * found elsewhere in documentation.
 */

object Language {

  trait SExp {
    /** Value of this expression in `env` */
    def eval(env: Env): SExp

    def opVal(env: Env): SExp = eval(env)

    /** Most expressions have no lvalue  */
    def lval(env: Env): LoadUpdate = throw RuntimeError(s"$this cannot be assigned to $position")

    /** Most expressions do not denote the empty list  */
    def isNull: Boolean = false

    /**
     * Evaluate an expression constructed as a quotation:
     */
    def evalQuote(env: Env): SExp = eval(env)

    /**
     * The position in source code at which this expression started.
     */
    var position: SourcePosition = SourcePosition("",-1,-1)

    def toPlainString: String = this.toString
  }

  abstract trait LoadUpdate extends SExp {
    def value: SExp
    def value_=(newValue: SExp): Unit = setValue(newValue)
    def setValue(newValue: SExp): Unit
  }

  case class Ref(name: String, var _value: SExp) extends LoadUpdate {
    override def eval(env: Env): SExp = value
    override def lval(env: Env): Ref = this
    def setValue(newValue: SExp): Unit = _value=newValue
    def value: SExp = _value
    override def toString: String = name
  }

  case class Variable(name: String)  extends SExp {
    var symbolic: Boolean = false

    def eval(env: Env): SExp = env(name) match {
      case None =>
        //  symbols are self-quoting when not bound
        if (symbolic)
           Quote(this)
        else
           throw RuntimeError(s"Unbound variable $name\n($position)")
      case Some(v) => v match {
        case lup: LoadUpdate => lup.value
        case _               => v
      }
    }

    override def lval(env: Env): LoadUpdate = env(name) match {
      case None => throw RuntimeError(s"Unbound variable $name\n($position)")
        case Some(v)   => v match {
        case lup: LoadUpdate   => lup
        case _                 => throw RuntimeError(s"Not an lvalue $name ($position)")
      }
    }

    override def toString = name
  }

  val nil = SExps(Nil)

  case class SExps(elements: List[SExp]) extends SExp {
    override def toString = elements.mkString("(", " ", ")")
    override def toPlainString = elements.map(_.toPlainString).mkString("(", " ", ")")
    override def isNull = elements.isEmpty

    def withErrorHandling(value: => SExp): SExp = {
        try value catch  {
          case exn: RuntimeError => throw RuntimeError(s"${exn.getMessage}\nin\n$this $position")
          case exn: SyntaxError  => throw SyntaxError(s"${exn.getMessage}\nin\n$this $position")
          case exn: MatchError   => throw SyntaxError(s"Number or type(s) of argument(s) wrong\nwhile evaluating\n$this $position")
          case exn => exn.printStackTrace(); throw SyntaxError(s"${exn.getMessage}\nin\n$this $position")
        }
    }


    def eval(env0: Env): SExp =
      if (elements.isEmpty) nil else {
          val operator = elements.head.opVal(env0)
          val result =
            operator match {
              case PositionSubr(_, scala) =>
                withErrorHandling {
                  val args = elements.tail.map(_.eval(env0))
                  scala(Str(position.toString) :: args)
                }
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

              case ExprAll(env1, variable, body) =>
                withErrorHandling {
                  val args = elements.tail.map(_.eval(env0))
                  body.eval(env1.extend(variable, args))
                }


              case FExpr(env1, params, body) =>
                withErrorHandling {
                  val args = elements.tail
                  body.eval(env1.extend(params, EnvExpr(env0)::args))
                }

              case FExprAll(env1, Pair(envArg: Variable, argsArg: Variable), body) =>
                withErrorHandling {
                  val args = elements.tail
                  body.eval(env1.extend(SExps(List(envArg, argsArg)), List(EnvExpr(env0), SExps(args))))
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

  case class Pair(l: SExp, r: SExp) extends SExp {
   override def toString: String = s"($l . $r)"
   def eval(env: Env): SExp = new ConstPair(l.eval(env), r.eval(env))
  }

  class ConstPair(l: SExp, r: SExp) extends Pair(l, r) with Const {
    override def eval(env: Env): Const = this
  }

  case class Num(value: Long) extends Const {
    override def toString = value.toString
  }

  /**
   * A `Hex` is just a number that is printed in hexadecimal.
   * `Hex` numbers combine arithmetically with numbers to form `Hex` numbers.
   */
  class Hex(_value: Long) extends Num(_value) {
    override def toString = f"0x$value%08x"
  }


  case class Bool(value: Boolean) extends Const {
    override def toString = value.toString
  }

  case class Str(value: String) extends Const {
    override def toString = s"\"$value\""
    override def toPlainString = value
  }

  case class Expr(env: Env, pattern: SExp, body: SExp) extends Const {
    override def toString = s"(fun $pattern $body)"
  }

  case class ExprAll(env: Env, pattern: Variable, body: SExp) extends Const {
    override def toString = s"(fun $pattern $body)"
  }

  case class FExpr(env: Env, pattern: SExp, body: SExp) extends Const {
    override def toString = s"(form $pattern $body)"
  }

  case class FExprAll(env: Env, pattern: Pair, body: SExp) extends Const {
    override def toString = s"(form $pattern $body)"
  }

  case class EnvExpr(env: Env) extends Const {
    override def toString: String = {
      val maplets = env.maplets.map { case (k, v) => s"\"$k\" -> $v" }
      maplets.mkString("(map ", " ", ")")
    }
  }

  case class Subr(name: String, scala: List[SExp] => SExp) extends Const {
    override def toString = name // s"Strict: $name"
  }

  // Invoked with the position of its call as its first argument
  case class PositionSubr(name: String, scala: List[SExp] => SExp) extends Const {
    override def toString = name // s"Strict: $name"
  }

  case class FSubr(name: String, scala: (Env, SExp) => SExp) extends Const {
    override def toString = name // s"Lazy: $name"
  }


  case class Opaque(value: Any) extends Const {
    override def toString = s"$value"
  }

  case class Quote(value: SExp) extends SExp {
    override def toString = s"`$value"
    override def toPlainString = value.toPlainString
    override def eval(env: Env): SExp = value
  }

  case object Nothing extends Const {
    override def toString = ""
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
