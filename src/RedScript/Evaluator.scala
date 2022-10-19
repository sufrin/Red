package RedScript

import scala.language.postfixOps

/**
 *  A global evaluator that defines syntactic forms as well as the semantics of
 *  primitive functions.
 */
class Evaluator {
  import RedScript.Language._

  /**
   * A multi-branched conditional expression takes the form:
   *
   * `(if' (cond thenExpr)*)` -- zero or more (condition, expr) pairs
   *
   * It yields `Nothing` if no condition yields true; else
   * it yields the value of the `thenExpr` paired with the first
   * of the conditions that yields `true`.
   */
  def evCond(env: Env, body: SExp): Const = {
    def evCases(cases: List[SExp]): Const = cases match {
      case Nil => Nothing

      case SExps(List(pred, thenPart)) :: cases =>
        pred.eval(env) match {
          case Bool(true) => thenPart.eval(env)
          case Bool(false) => evCases(cases)
          case other => throw RuntimeError(s"condition evaluates to a non-Bool: $other", Some(pred))
        }

      case other => throw SyntaxError(s"Malformed if': ${cases.mkString(" ")}")
    }
    body match {
      case SExps(cases) => evCases(cases)
      case other => throw SyntaxError(s"Malformed conditional (if $body)")
    }
  }

  /** A single-branched conditional expression takes the form:
   *
   *  `(if condition thenExpr elseExpr)` -- a condition followed by two expressions
   *
   *  It yields the same as
   *
   *  `(if' (condition thenExpr) (true elseExpr))`
   */
  def evIf(env: Env, body: SExp): Const = body match {
    case SExps(List(pred, thenPart, elsePart)) => pred.eval(env) match {
      case Bool(true) => thenPart.eval(env)
      case Bool(false) => elsePart.eval(env)
      case other => throw RuntimeError(s"Non-Bool condition: $other in (if $pred $thenPart $elsePart)", Some(pred))
    }
    case other => throw SyntaxError(s"Malformed if: (if $other)")
  }

  /**
   * User-declared constants and variables
   */
  val global    = new MutableEnv
  /**
   * Built-in named unchangeable forms
   */
  val syntaxEnv = new MutableEnv

  def evSet(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(lvalue, rvalue)) =>
        lvalue.lval(env).value = rvalue.eval(env);
        Nothing
      case other => throw SyntaxError(s"Malformed assignment: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evGlobal(isVar: Boolean)(env: Env, body: SExp): Const = {
    body match {
      case SExps(List(Variable(name), value)) =>
           val v = value.eval(env)
           global.define(name, if (isVar) Ref(name, v) else v)
           Nothing
      case other => throw SyntaxError(s"Malformed global declaration: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evLet(isVar: Boolean)(env: Env, form: SExp): Const = {
    form match {
      case SExps(pairs) if pairs.length >= 2 =>
        try {
          val bindings = (for {i <- 0 until pairs.length - 1} yield pairs(i)).toList
          val bvs = bindings.map { case SExps(List(bv, _)) => bv }
          // val bvs = (for { SExps(List(bv, _)) <- bindings } yield bv).toList
          val args =
            if (isVar)
              for {SExps(List(bv, expr)) <- bindings} yield Ref(bv.toString, expr.eval(env))
            else
              for {SExps(List(_, expr)) <- bindings} yield expr.eval(env)

          val body = pairs.last
            body.eval(env.extend(SExps(bvs), args.toList))
        } catch {
          case exn: MatchError => throw SyntaxError(s"Malformed declaration")
        }

      case other => throw SyntaxError(s"Malformed declaration")
    }
  }

  @inline def isAtom(pattern: SExp): Boolean = pattern match { case Variable(_) => true ; case _ => false }
  @inline def isPattern(pattern: SExp): Boolean = pattern match {
    case SExps(params) if params.forall(isAtom) => true
    case _             if isAtom(pattern)       => true
    case _                                      => false
  }

  def evDef(env: Env, form: SExp): Const = {
    form match {
      case SExps(Variable(name) :: pattern :: body) if isPattern(pattern) => global.define(name, Expr(global, pattern, mkSequential(body))); Nothing
      case _ => throw SyntaxError(s"Malformed definition")
    }
  }

  def mkAtom(name: String, pos: SourcePosition): Variable = {
    val atom = Variable(name)
    atom.position=pos
    atom
  }

  /**  Workhorse to implement the transformations (for N>1):
   *
   *   {{{
   *   (expr params e1 e2 eN) => (expr params (Seq e1 e2 eN))
   *   }}}
   *   and
   *
   *   {{{
   *   (def f(params) e1 e2 eN) => (def  f(params) (Seq e1 e2 eN))
   *   }}}
   */
  def mkSequential(exprs: List[SExp]): SExp = exprs match {
    case List(expr) => expr
    case exprs => SExps(syntaxEnv("seq").get :: exprs)
  }

  def evFun(env: Env, form: SExp): Const = {
    form match {
      case SExps(pattern :: body) if isPattern(pattern) => Expr(env, pattern, mkSequential(body))
      case SExps(pattern :: body) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function body: $form")
    }
  }

  /** Lazy conjunction of `test` applied to the value of each argument */
  def forall(name: String)(test: SExp => Boolean): Const =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.forall(expr => test(expr.eval(env))))})

  /** Lazy disjunction of `test` applied to the value of each argument */
  def exists(name: String)(test: SExp => Boolean): Const =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.exists(expr => test(expr.eval(env))))})

  /** Application of `op` to the arguments as a whole */
  def fun(name: String, op: List[Const]=>Const): Subr  =  Subr(name, op)

  /** `op`-(left)reduction of the arguments as a whole */
  def red(name: String, op: (Int,Int)=>Int):Subr = {
    val opn: (Const,Const)=>Const  = { case (Num(a),Num(b)) => Num(op(a,b)) }
    fun(name, { case args: List[Const] if (args.nonEmpty) => args.reduceLeft(opn(_,_)) })
  }

  /**
   * `op`-(left)reduction of the arguments as a whole; unless there is a single argument, in which case `op(z, arg)`
   *
   * This permits unary negation straightforwardly
   * Thus: (- x) means negated x, and (- x1 x2 ...) means (x1-x2)-x3)-...
   */
  def minus: Subr = {
    val opn: (Const,Const)=>Const  = { case (Num(a),Num(b)) => Num(a-b) }
    fun("-", { case List(Num(arg))    => Num(- arg)
               case args: List[Const] => args.reduceLeft(opn(_,_)) }) }

  def rel(name: String, op: (Const, Const) => Boolean):Subr =
    fun(name, { case List(a: Const, b: Const) => Bool(op(a,b)) })

  /**
   *  The top-level environment binds, as constants, names subject to "just-in-time"
   *  translation in the parser. When any of the names defined here appear
   *  in an expression they are JIT-translated into their value. This
   *  means that the cost of evaluating a predefined language form is not
   *  dependent on the length of the environment at the time of its evaluation.
   */
  val primitives: List[(String, Const)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null",       { case List(Seq(Nil)) => Bool(true); case List(_) => Bool(false); case _ => throw SyntaxError("null requires an argument") }),
    "hd"        -> Subr("hd",         { case List(Seq((h::t))) => h; case List(Seq(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl",         { case List(Seq((h::t))) => Seq(t); case List(Seq(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "cons"      -> Subr("cons",       { case List(const, Seq(consts)) => Seq(const :: consts) } ),
    "++"        -> Subr("++",         { case List(Seq(k0), Seq(k1)) => Seq(k0++k1); case other => throw RuntimeError(s"malformed ++: ${SExps(other)}") } ),
    "fun"       -> FSubr ("fun",      evFun),
    ":="        -> FSubr (":=",       evSet),
    "variable"  -> FSubr ("variable", evGlobal(true)),
    "constant"  -> FSubr ("constant", evGlobal(false)),
    "var"       -> FSubr ("var",      evLet(true)),
    "val"       -> FSubr ("val",      evLet(false)),
    "if'"       -> FSubr ("if'",      evCond),
    "if"        -> FSubr ("if",       evIf),
    "def"       -> FSubr ("def",      evDef),
    "seq"       -> Subr  ("seq",      { case Nil => Nothing; case args => args.last }),
    "print"     -> Subr  ("print",    { case args => args.foreach{ case k => print(k.show) } ; Console.flush(); Nothing }),
    "println"   -> Subr  ("println",  { case Nil => println(); Nothing; case args => args.foreach{ case k => println(k.show) } ; Console.flush(); Nothing }),
    "?"         -> Subr  ("?",        { case args => args.foreach{ case k => print(k.show); print(" ") }; Console.flush(); args.last}),
    "list"      -> Subr  ("list",     { args => Seq(args)}),
    "isAtom"    -> forall("isAtom")   { case Quote(Variable(_)) => true; case _ => false },
    "isSymb"    -> forall("isSymb")   { case Quote(v@Variable(_)) => v.symbolic; case _ => false },
    "isVar"     -> forall("isVar")    { case Quote(v@Variable(_)) => !v.symbolic; case _ => false },
    "isNum"     -> forall("isNum")    { case Num(_)=>true; case _ => false },
    "isList"    -> forall("isList")   { case Seq(_)=>true; case _ => false },
    "isString"  -> forall("isString") { case Str(_)=>true; case _ => false },
    "toString"  -> Subr("toString",   { case List(const) => Str(const.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "eval"      -> FSubr("eval",      { case (env, SExps(List(expr))) => expr.eval(env).evalQuote(env)}),
    "&&"        -> forall("&&")       { case Bool(b)=> b },
    "||"        -> exists("||")       { case Bool(b)=> b },
    "+"         -> red("+",   (_.+(_))),
    "-"         -> minus, // special treatment of unary negation
    "*"         -> red("*",   (_.*(_))),
    "/"         -> red("/",   (_./(_))),
    "="         -> rel("=",   (_.equals(_))),
    "<"         -> rel("<",   { case (Num(a), Num(b))=>a<b;  case (Str(a), Str(b))=>a<b;  case (Bool(a), Bool(b))=>a<b  }),
    "<="        -> rel("<=",  { case (Num(a), Num(b))=>a<=b; case (Str(a), Str(b))=>a<=b; case (Bool(a), Bool(b))=>a<=b }),
    ">"         -> rel("<",   { case (Num(a), Num(b))=>a>b;  case (Str(a), Str(b))=>a<b;  case (Bool(a), Bool(b))=>a>b  }),
    ">="        -> rel("<=",  { case (Num(a), Num(b))=>a>=b; case (Str(a), Str(b))=>a<=b; case (Bool(a), Bool(b))=>a>=b }),
    "true"      -> Bool(true),
    "false"     -> Bool(false),
  )

  def run(sexp: SExp): Const =
    try sexp.eval(global) catch {
      case exn: scala.Error => Str(s"RuntimeError: ${exn.getMessage}")
    }

  for { (name, value) <- primitives } syntaxEnv.define(name, value)


  def rep(source: String, show: Boolean=true): Unit = readEvalPrint(new Parser(io.Source.fromString(source)), show)

  def readEvalPrint(parser: Parser,
                    show:  Boolean =  true,
                    showNormal:  String => Unit = Console.print(_),
                    showError:      String => Unit = Console.println(_)): Unit = {
    parser.syntaxEnv=syntaxEnv // for JIT compilation of operators
    println(s"Reading ${parser.position}")
    try {
      while (parser.nextSymb() != Lexical.EOF) try {
        val e = parser.read
        if (show) showNormal(s"${e.position}: $e => ")
        val r = try run(e).toString catch {
          case exn: RuntimeError => exn
          case exn: SyntaxError => exn
        }
        showNormal(s"$r\n")
      } catch {
        case exn: SyntaxError => showError(exn.toString)
      }
    }
    catch {
      case exn: SyntaxError => showError(exn.toString)
      case exn: Exception => showError(exn.toString)
    }
  }



}

