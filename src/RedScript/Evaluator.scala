package RedScript

import scala.language.postfixOps

/**
 *  A global evaluator that defines syntactic forms as well as the semantics of
 *  primitive functions.
 */
class Evaluator {
  import RedScript.Language._

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

  val global    = new MutableEnv
  val syntaxEnv = new MutableEnv

  def evSet(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(lvalue, rvalue)) =>
        lvalue.lval(env).value = rvalue.eval(env);
        nil
      case other => throw SyntaxError(s"Malformed assignment: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evGlobal(isVar: Boolean)(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(Variable(name), value)) =>
           val v = value.eval(env)
           global.define(name, if (isVar) Ref(name, v) else v)
           nil
      case other => throw SyntaxError(s"Malformed global declaration: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evLet(isVar: Boolean)(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(bv, value, body)) =>
        val v = value.eval(env)
        val params = SExps(List(bv))
        val args   = List(if (isVar) Ref(bv.toString, v) else v)
        body.eval(env.extend(params, args))
      case other => throw SyntaxError(s"Malformed let declaration: $other")
    }
  }

  @inline def isAtom(pattern: SExp): Boolean = pattern match { case Variable(_) => true ; case _ => false }
  @inline def isPattern(pattern: SExp): Boolean = pattern match {
    case SExps(params) if params.forall(isAtom) => true
    case _             if isAtom(pattern)       => true
    case _                                      => false
  }

  def evDef(env: Env, body: SExp): Const = {
    body match {
      case SExps(Variable(name) :: pattern :: body:: Nil) if isPattern(pattern) => global.define(name, Expr(global, pattern, body)); nil
      case SExps(Variable(name) :: pattern :: body:: rest) if isPattern(pattern) => global.define(name, Expr(global, pattern, SExps(mkAtom("seq", body.position) :: body:: rest))); nil
      case SExps(List(Variable(name), pattern, body)) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s) in definition: ")
      case _ => throw SyntaxError(s"Malformed definition")
    }
  }

  def mkAtom(name: String, pos: SourcePosition): Variable = {
    val atom = Variable(name)
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

  // Needs to be lazy
  def forall(name: String)(test: SExp => Boolean): Const =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.forall(expr => test(expr.eval(env))))})

  def exists(name: String)(test: SExp => Boolean): Const =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.exists(expr => test(expr.eval(env))))})

  def fun(name: String, op: List[Const]=>Const): Subr  =
    Subr(name, { case args => try op(args) catch { case exn: MatchError => throw RuntimeError(s"Badly typed or mismatched arguments to $name: ${Seq(args)}")}})

  def red(name: String, op: (Int,Int)=>Int):Subr = {
    val opn: (Const,Const)=>Const  = { case (Num(a),Num(b)) => Num(op(a,b)) }
    fun(name, { args: List[Const] => args.reduceLeft(opn(_,_)) })
  }

  def red1(name: String, op: (Int,Int)=>Int):Subr = {
    val opn: (Const,Const)=>Const  = { case (Num(a),Num(b)) => Num(op(a,b)) }
    fun(name, { case List(Num(arg)) => Num(op(0, arg)); case args: List[Const] => args.reduceLeft(opn(_,_)) })
  }

  def rel(name: String, op: (Const, Const) => Boolean):Subr = {
    fun(name, { case List(a: Const, b: Const) => Bool(op(a,b)) })
  }

  /**
   *  The top-level environment binds, as constants, names subject to "just-in-time"
   *  translation in the parser. When any of the names defined here appear
   *  in an expression they are JIT-translated into their value. This
   *  means that the cost of evaluating a predefined language form is not
   *  dependent on the length of the environment at the time of its evaluation.
   */
  val primitives: List[(String, Const)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null", { case List(Seq(Nil)) => Bool(true); case List(_) => Bool(false); case other => throw RuntimeError(s"malformed null: $other") }),
    "hd"        -> Subr("hd", { case List(Seq((h::t))) => h; case List(Seq(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl", { case List(Seq((h::t))) => Seq(t); case List(Seq(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "cons"      -> Subr("cons", { case List(const, Seq(consts)) => Seq(const :: consts); case other => throw RuntimeError(s"malformed cons: ${SExps(other)}") } ),
    "fun"       -> FSubr ("fun", evFun),
    ":="        -> FSubr (":=", evSet),
    "variable"  -> FSubr ("variable", evGlobal(true)),
    "constant"  -> FSubr ("constant", evGlobal(false)),
    "var"       -> FSubr ("var", evLet(true)),
    "val"       -> FSubr ("val", evLet(false)),
    "if'"       -> FSubr ("if'", evCond),
    "if"        -> FSubr ("if", evIf),
    "def"       -> FSubr ("def", evDef),
    "seq"       -> Subr  ("seq", { case Nil => nil; case args => args.last }),
    "print"     -> Subr  ("print", { case args => args.foreach(println(_)); nil}),
    "?"          -> Subr  ("?",    { case args => args.foreach(println(_)); args.last}),
    "printenv"  -> FSubr  ("printenv", { case (env, SExps(Nil)) => env.print(); nil
                                         case (env, SExps(List(exp))) => env.print(); exp.eval(env)
                                       }),
    "list"      -> Subr  ("list", { args => Seq(args)}),
    "isAtom"    -> forall("isAtom") { case Variable(_)=>true; case Symbol(_) => true; case _ => false },
    "isVar"     -> forall("isVar") { case Variable(_)=>true; case _ => false },
    "isSymb"    -> forall("isSymb") { case Symbol(_) => true; case _ => false },
    "isNum"     -> forall("isNum")  { case Num(_)=>true; case _ => false },
    "isList"    -> forall("isList") { case Seq(_)=>true; case _ => false },
    "isString"  -> forall("isString") { case Str(_)=>true; case _ => false },
    "toString"  -> Subr("toString", { case List(const) => Str(const.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "eval"      -> FSubr("eval", { case (env, SExps(List(expr))) => expr.eval(env).evalQuote(env)}),
    "+"         -> red("+", (_.+(_))),
    "-"         -> red1("-", (_.-(_))),
    "*"         -> red("*", (_.*(_))),
    "/"         -> red("/", (_./(_))),
    "="         -> rel("=", (_.equals(_))),
    "true"      -> Bool(true),
    "false"     -> Bool(false),
    "&&"        -> forall("&&"){ case Bool(b)=> b },
    "||"        -> exists("||"){ case Bool(b)=> b },
  )

  def run(sexp: SExp): Const =
    try sexp.eval(global) catch {
      case exn: scala.Error => Str(s"RuntimeError: ${exn.getMessage}")
    }

  for { (name, value) <- primitives } syntaxEnv.define(name, value)


  def rep(source: String): Unit = rep(new Parser(io.Source.fromString(source)))

  def rep(parser: Parser): Unit = {
    parser.syntaxEnv=syntaxEnv // for JIT compilation of operators
    try {
      while (parser.nextSymb() != Lexical.EOF) try {
        val e = parser.read
        val r = try run(e).toString catch {
          case exn: RuntimeError => exn
        }
        println(s"$e => $r")
      } catch {
        case exn: SyntaxError => println(exn)
      }
    }
    catch {
      case exn: SyntaxError => println(exn)
      case exn: Exception => println(exn)
    }
  }

  def rp(source: String): Unit = {
    val p = new Parser(io.Source.fromString(source))
    while (p.nextSymb() != Lexical.EOF) {
      try {
        val e = p.read
        println(s"${e.position}: $e => ")
      } catch {
        case exn: SyntaxError => println(s"${p.position}: $exn")
      }
    }
  }

}

