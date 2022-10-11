package RedLisp


class Global[T] {
  import RedLisp.Syntax._
  val nil = Seq[T](Nil)

  /**
   * A cases expression takes one of the following forms:
   *
   * `(if* (condition thenExpr)*)` - zero or more condition, expr pairs
   *
   * `(if* (condition thenExpr)* elseExpr)` - zero or more condition, expr pairs, followed by an expr
   *
   */
  def evCond(env: Env[T], body: SExp[T]): Const[T] = {
    def evCases(cases: List[SExp[T]]): Const[T] = cases match {
      case Nil => nil

      case SExps(List(pred, thenPart)) :: cases =>
        pred.eval(env) match {
          case Bool(true) => thenPart.eval(env)
          case Bool(false) => evCases(cases)
          case other => throw Error(s"condition evaluates to a non-Bool: $other", Some(pred))
        }

      case List(lastPart) => lastPart.eval(env)

      case other => throw Error(s"malformed conditional ${cases.mkString(" ")}")
    }
    body match {
      case SExps(cases) => evCases(cases)
      case other => throw Error(s"malformed conditional (if $body)")
    }
  }

  /**
   *  `(if condition thenExpr elseExpr)` - a condition followed by two expressions
   */
  def evIf(env: Env[T], body: SExp[T]): Const[T] = body match {
    case SExps(List(pred, thenPart, elsePart)) => pred.eval(env) match {
      case Bool(true) => thenPart.eval(env)
      case Bool(false) => elsePart.eval(env)
      case other => throw Error(s"condition evaluates to a non-Bool: $other", Some(pred))
    }
    case other => throw Error(s"malformed conditional (if $other)")
  }

  val global = new MutableEnv[T]

  def evSet(env: Env[T], body: SExp[T]): Const[T] = {
    body match {
      case SExps(List(Atom(name), value)) => global.set(name, value.eval(env)); nil
      case other => throw Error(s"malformed assignment $other")
    }
  }

  def evVar(env: Env[T], body: SExp[T]): Const[T] = {
    body match {
      case SExps(List(Atom(name), value)) => global.define(name, value.eval(env)); nil
      case other => throw Error(s"malformed var declaration $other")
    }
  }

  def evDef(env: Env[T], body: SExp[T]): Const[T] = {
    body match {
      case SExps(List(Atom(name), pattern, body)) => global.define(name, Expr(global, pattern, body)); nil
      case other => throw Error(s"malformed definition $other")
    }
  }

  def evFun[T](env: Env[T], body: SExp[T]): Const[T] = {
    body match {
      case SExps(params :: body :: Nil) => Expr(env, params, body)
      case other => throw Error(s"malformed function body $other")
    }
  }

  def predSubr(name: String)(test: SExp[T]=>Boolean): Const[T] =
    Subr(name, { args => Bool(args.forall(test)) })


  val primitives: List[(String, Const[T])] = List(
    "nil"       -> nil,
    "null"      -> Subr("null", { case List(Seq(Nil)) => Bool(true); case List(_) => Bool(false); case other => throw Error(s"malformed null") }),
    "cons"      -> Subr("cons", { case List(const, Seq(consts)) => Seq(const :: consts); case other => throw Error(s"malformed cons")}),
    "fun"       -> FSubr ("fun", evFun),
    "set"       -> FSubr ("set", evSet),
    "var"       -> FSubr ("var", evVar),
    "if*"       -> FSubr ("if*", evCond),
    "if"        -> FSubr ("if", evIf),
    "def"       -> FSubr ("def", evDef),
    "seq"       -> Subr  ("seq", { case Nil => nil; case args => args.last }),
    "print"     -> Subr  ("print", { case args => args.foreach(println(_)); nil}),
    "printenv"  -> FSubr  ("printenv", { case (env, args) => env.print(); nil }),
    "list"      -> Subr  ("list", { args => Seq(args)}),
    "isAtom"    -> predSubr("isAtom") { case Atom(_)=>true; case _ => false },
    "isNum"     -> predSubr("isNum")  { case Num(_)=>true; case _ => false },
    "isList"    -> predSubr("isList") { case Seq(_)=>true; case _ => false },
    "isString"  -> predSubr("isString") { case Str(_)=>true; case _ => false },
    "toString"  -> Subr("toString", { case List(const) => Str(const.toString);  case other => throw Error(s"malformed toString") }),
    "'"         -> FSubr ("'",  { case (env, s) => Quote(s) })
  )

  def Run(sexp: SExp[T]): Const[T] =
    try sexp.eval(global) catch {
      case exn: scala.Error =>
        exn.printStackTrace()
        nil
    }

  for { (name, value) <- primitives } global.define(name, value)
}

