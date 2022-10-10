package RedScript

/**
 *  A global evaluator that defines primitives
 */
class Runtime {
  import RedScript.Syntax._
  val nil = Seq(Nil)

  /**
   * A cases expression takes one of the following forms:
   *
   * `(if* (condition thenExpr)*)` - zero or more condition, expr pairs
   *
   * `(if* (condition thenExpr)* elseExpr)` - zero or more condition, expr pairs, followed by an expr
   *
   */
  def evCond(env: Env, body: SExp): Const = {
    def evCases(cases: List[SExp]): Const = cases match {
      case Nil => nil

      case SExps(List(pred, thenPart)) :: cases =>
        pred.eval(env) match {
          case Bool(true) => thenPart.eval(env)
          case Bool(false) => evCases(cases)
          case other => throw RuntimeError(s"condition evaluates to a non-Bool: $other", Some(pred))
        }

      case Nil => Seq(Nil)

      case other => throw SyntaxError(s"Malformed if*: ${cases.mkString(" ")}")
    }
    body match {
      case SExps(cases) => evCases(cases)
      case other => throw SyntaxError(s"Malformed conditional (if $body)")
    }
  }

  /**
   *  `(if condition thenExpr elseExpr)` - a condition followed by two expressions
   */
  def evIf(env: Env, body: SExp): Const = body match {
    case SExps(List(pred, thenPart, elsePart)) => pred.eval(env) match {
      case Bool(true) => thenPart.eval(env)
      case Bool(false) => elsePart.eval(env)
      case other => throw RuntimeError(s"Non-Bool condition: $other in (if $pred $thenPart $elsePart)", Some(pred))
    }
    case other => throw SyntaxError(s"Malformed if: (if $other)")
  }

  val global = new MutableEnv

  def evSet(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(Atom(name), value)) => global.set(name, value.eval(env)); nil
      case other => throw SyntaxError(s"Malformed assignment: $other")
    }
  }

  def evVar(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(Atom(name), value)) => global.define(name, value.eval(env)); nil
      case other => throw SyntaxError(s"Malformed var declaration: $other")
    }
  }

  @inline def isAtom(pattern: SExp): Boolean = pattern match { case Atom(_) => true ; case _ => false }
  @inline def isPattern(pattern: SExp): Boolean = pattern match {
    case SExps(params) if params.forall(isAtom) => true
    case _             if isAtom(pattern)       => true
    case _                                      => false
  }

  def evDef(env: Env, body: SExp): Const = {
    body match {
      case SExps(Atom(name) :: pattern :: body:: Nil) if isPattern(pattern) => global.define(name, Expr(global, pattern, body)); nil
      case SExps(Atom(name) :: pattern :: body:: rest) if isPattern(pattern) => global.define(name, Expr(global, pattern, SExps(mkAtom("seq", body.position) :: body:: rest))); nil
      case SExps(List(Atom(name), pattern, body)) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s) in definition: ")
      case _ => throw SyntaxError(s"Malformed definition")
    }
  }

  def mkAtom(name: String, pos: SourcePosition): Atom = {
    val atom = Atom(name)
    atom.position=pos
    atom
  }

  def evFun(env: Env, body: SExp): Const = {
    body match {
      case SExps(pattern :: body :: Nil)  if isPattern(pattern) => Expr(env, pattern, body)
      case SExps(pattern :: body :: rest) if isPattern(pattern) => Expr(env, pattern, SExps(mkAtom("seq", body.position) :: body:: rest))
      case SExps(pattern :: rest) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s) in abstraction: ")
      case _ => throw SyntaxError(s"malformed function body: $body")
    }
  }

  def predSubr(name: String)(test: SExp=>Boolean): Const =
    Subr(name, { args => Bool(args.forall(test)) })


  val primitives: List[(String, Const)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null", { case List(Seq(Nil)) => Bool(true); case List(_) => Bool(false); case other => throw RuntimeError(s"malformed null: $other") }),
    "hd"        -> Subr("hd", { case List(Seq((h::t))) => h; case List(Seq(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl", { case List(Seq((h::t))) => Seq(t); case List(Seq(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "cons"      -> Subr("cons", { case List(const, Seq(consts)) => Seq(const :: consts); case other => throw RuntimeError(s"malformed cons: ${SExps(other)}") } ),
    "fun"       -> FSubr ("fun", evFun),
    "set"       -> FSubr ("set", evSet),
    "var"       -> FSubr ("var", evVar),
    "if*"       -> FSubr ("if*", evCond),
    "if"        -> FSubr ("if", evIf),
    "def"       -> FSubr ("def", evDef),
    "seq"       -> Subr  ("seq", { case Nil => nil; case args => args.last }),
    "print"     -> Subr  ("print", { case args => args.foreach(println(_)); nil}),
    "?"          -> Subr  ("?",    { case args => args.foreach(println(_)); args.last}),
    "printenv"  -> FSubr  ("printenv", { case (env, SExps(Nil)) => env.print(); nil
                                         case (env, SExps(List(exp))) => env.print(); exp.eval(env)
                                       }),
    "list"      -> Subr  ("list", { args => Seq(args)}),
    "isAtom"    -> predSubr("isAtom") { case Atom(_)=>true; case _ => false },
    "isNum"     -> predSubr("isNum")  { case Num(_)=>true; case _ => false },
    "isList"    -> predSubr("isList") { case Seq(_)=>true; case _ => false },
    "isString"  -> predSubr("isString") { case Str(_)=>true; case _ => false },
    "toString"  -> Subr("toString", { case List(const) => Str(const.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "eval"      -> FSubr("eval", { case (env, SExps(List(expr))) => expr.eval(env).evalQuote(env)}),
  )

  def Run(sexp: SExp): Const =
    try sexp.eval(global) catch {
      case exn: scala.Error => Str(s"RuntimeError: ${exn.getMessage}")
    }

  for { (name, value) <- primitives } global.define(name, value)
}

