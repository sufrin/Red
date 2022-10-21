package RedScript

import java.io.FileInputStream
import java.nio.file.Path
import scala.io.BufferedSource
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
  def evCond(env: Env, body: SExp): SExp = {
    def evCases(cases: List[SExp]): SExp = cases match {
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
  def evIf(env: Env, body: SExp): SExp = body match {
    case SExps(List(pred, thenPart, elsePart)) => pred.eval(env) match {
      case Bool(true) => thenPart.eval(env)
      case Bool(false) => elsePart.eval(env)
      case other => throw RuntimeError(s"Non-Bool condition: $other in (if $pred $thenPart $elsePart)", Some(pred))
    }
    case other => throw SyntaxError(s"Malformed if: (if $other)")
  }

  /**
   * User-declared SExpants and variables
   */
  val global    = new MutableEnv
  /**
   * Built-in named unchangeable forms
   */
  val syntaxEnv = new MutableEnv

  def evSet(env: Env, body: SExp): SExp = {
    body match {
      case SExps(List(lvalue, rvalue)) =>
        lvalue.lval(env).value = rvalue.eval(env);
        Nothing
      case other => throw SyntaxError(s"Malformed assignment: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evGlobal(isVar: Boolean)(env: Env, body: SExp): SExp = {
    body match {
      case SExps(List(Variable(name), value)) =>
           val v = value.eval(env)
           global.define(name, if (isVar) Ref(name, v) else v)
           Nothing
      case other => throw SyntaxError(s"Malformed global declaration: $other")
    }
  }

  // TODO: generalise to multiple declarations
  def evLet(isVar: Boolean)(env: Env, form: SExp): SExp = {
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
  @inline def isPattern(pattern: List[SExp]): Boolean = pattern.forall(isPattern(_))
  @inline def isPattern(pattern: SExp): Boolean = pattern match {
    case SExps(params) if params.forall(isAtom) => true
    case _             if isAtom(pattern)       => true
    case _                                      => false
  }

  def evDef(isFexpr: Boolean)(env: Env, form: SExp): SExp = {
    form match {
      //   def (f a b c) body
      case SExps(SExps(Variable(name) :: pattern) :: body) if isPattern(pattern) =>
           global.define(name, (if (isFexpr) FExpr else Expr)(env, SExps(pattern) , mkSequential(body)))
           Nothing
      // def f all body
      case SExps(Variable(name) :: (allArgs@Variable(_)) :: body) =>
           global.define(name, (if (isFexpr) FExpr else Expr)(global, allArgs, mkSequential(body)))
           Nothing
      case _ => throw SyntaxError(s"Malformed definition: should be (def (f (args) ...)) or (def f arg ...)")
    }
  }

  def evFun(env: Env, form: SExp): SExp = {
    form match {
      // expr (a b c) body
      case SExps(pattern :: body) if isPattern(pattern) => Expr(env, pattern, mkSequential(body))
      // expr all bosy
      case SExps(pattern :: body) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function expression: $form")
    }
  }

  def evShow(args: List[SExp]): SExp =  {
    args match {
      case Nil => Str("")
      case List(c) => Str(c.show)
      case cs =>
        Str(cs.map(_.show).mkString("[", " ", "]"))
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



  /** Lazy conjunction of `test` applied to the value of each argument */
  def forall(name: String)(test: SExp => Boolean): SExp =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.forall(expr => test(expr.eval(env))))})

  /** Lazy disjunction of `test` applied to the value of each argument */
  def exists(name: String)(test: SExp => Boolean): SExp =
    FSubr(name, { case (env, SExps(exprs)) => Bool(exprs.exists(expr => test(expr.eval(env))))})

  /** Application of `op` to the arguments as a whole */
  def fun(name: String, op: List[SExp]=>SExp): Subr  =  Subr(name, op)

  /** `op`-(left)reduction of the arguments as a whole */
  def red(name: String, op: (Int,Int)=>Int):Subr = {
    val opn: (SExp,SExp)=>SExp  = { case (Num(a),Num(b)) => Num(op(a,b)) }
    fun(name, { case args: List[SExp] if (args.nonEmpty) => args.reduceLeft(opn(_,_)) })
  }

  /** `op`-(left)reduction of the arguments as a whole */
  def redString(name: String, op: (String,String)=>String):Subr = {
    val opn: (SExp,SExp)=>SExp  = { case (a,b) => Str(op(a.show, b.show)) }
    fun(name, { case args: List[SExp] if (args.nonEmpty) => args.reduceLeft(opn(_,_)) })
  }

  /**
   * `op`-(left)reduction of the arguments as a whole; unless there is a single argument, in which case `op(z, arg)`
   *
   * This permits unary negation straightforwardly
   * Thus: (- x) means negated x, and (- x1 x2 ...) means (x1-x2)-x3)-...
   */
  def minus: Subr = {
    val opn: (SExp,SExp)=>SExp  = { case (Num(a),Num(b)) => Num(a-b) }
    fun("-", { case List(Num(arg))    => Num(- arg)
               case args: List[SExp] => args.reduceLeft(opn(_,_)) }) }

  def rel(name: String, op: (SExp, SExp) => Boolean):Subr =
    fun(name, { case List(a: SExp, b: SExp) => Bool(op(a,b)) })

  def evalENV(args: List[SExp]): SExp = args match {
    case List(Str(variable), Str(default)) => Str(sys.env.getOrElse(variable, default))
    case List(Str(variable))               => Str(sys.env.getOrElse(variable, ""))
  }

  def evalPROP(args: List[SExp]): SExp = args match {
    case List(Str(variable), Str(default)) => Str(sys.props.getOrElse(variable, default))
    case List(Str(variable))               => Str(sys.props.getOrElse(variable, ""))
  }

  var position: SourcePosition = SourcePosition("",0,0)

  /**
   *  The top-level environment binds, as SExpants, names subject to "just-in-time"
   *  translation in the parser. When any of the names defined here appear
   *  in an expression they are JIT-translated into their value. This
   *  means that the cost of evaluating a predefined language form is not
   *  dependent on the length of the environment at the time of its evaluation.
   */
  val primitives: List[(String, SExp)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null",       { case List(SExps(Nil)) => Bool(true); case List(_) => Bool(false); case _ => throw SyntaxError("null requires an argument") }),
    "hd"        -> Subr("hd",         { case List(SExps((h::t))) => h; case List(SExps(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl",         { case List(SExps((h::t))) => SExps(t); case List(SExps(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "cons"      -> Subr("cons",       { case List(s, SExps(ss)) => SExps(s::ss) }),
    "++"        -> Subr("++",         { case List(SExps(k0), SExps(k1)) => SExps(k0++k1); case other => throw RuntimeError(s"malformed ++: ${SExps(other)}") } ),
    "fun"       -> FSubr ("fun",      evFun),
    ":="        -> FSubr (":=",       evSet),
    "variable"  -> FSubr ("variable", evGlobal(true)),
    "constant"  -> FSubr ("constant", evGlobal(false)),
    "var"       -> FSubr ("var",      evLet(true)),
    "val"       -> FSubr ("val",      evLet(false)),
    "if'"       -> FSubr ("if'",      evCond),
    "if"        -> FSubr ("if",       evIf),
    "def"       -> FSubr ("def",      evDef(false)),
    "def'"      -> FSubr ("def'",     evDef(true)),
    "seq"       -> Subr  ("seq",      { case Nil => Nothing; case args => args.last }),
    "println"   -> Subr  ("println",  { case args => args.foreach{ case k => normalFeedback(k.show); normalFeedback(" ") } ; normalFeedbackLn(""); Nothing }),
    "?"         -> Subr  ("?",        { case args => args.foreach{ case k => normalFeedback(k.show); normalFeedback(" ") }; args.last }),
    "show"      -> Subr  ("show",     evShow(_)),
    "list"      -> Subr  ("list",     { args => SExps(args)}),
    "isAtom"    -> forall("isAtom")   { case Quote(Variable(_)) => true; case _ => false },
    "isSymb"    -> forall("isSymb")   { case Quote(v@Variable(_)) => v.symbolic; case _ => false },
    "isVar"     -> forall("isVar")    { case Quote(v@Variable(_)) => !v.symbolic; case _ => false },
    "isNum"     -> forall("isNum")    { case Num(_)=>true; case _ => false },
    "isList"    -> forall("isList")   { case SExps(_)=>true; case _ => false },
    "isString"  -> forall("isString") { case Str(_)=>true; case _ => false },
    "toString"  -> Subr("toString",   { case List(sexp) => Str(sexp.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "eval"      -> FSubr("eval",      { case (env, SExps(List(expr))) => expr.eval(env).evalQuote(env)}),
    "&&"        -> forall("&&")       { case Bool(b)=> b },
    "||"        -> exists("||")       { case Bool(b)=> b },
    "string"    -> redString("string", (_.+(_))),
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
    "ENV"       -> Subr("ENV",   evalENV),
    "PROP"      -> Subr("PROP",  evalPROP),
    "SOURCE"    -> Subr("SOURCE",  { case Nil => Str(position.toString) })
  )

  def run(sexp: SExp): SExp = {
    position = sexp.position
    try sexp.eval(global) catch {
      case exn: scala.Error => Str(s"Runtime Error: ${exn.getMessage}")
    }
  }

  for { (name, value) <- primitives } syntaxEnv.define(name, value)

  def normalFeedback(s: String): Unit   = { Console.print(s); Console.flush() }
  def normalFeedbackLn(s: String): Unit = { Console.println(s); Console.flush() }
  def errorFeedback(s: String): Unit    = { Console.println(s); Console.flush() }

  def readEvalPrint(path: Path, show:  Boolean =  true): Unit = {
    val file   = path.toFile
    val source = new BufferedSource(new FileInputStream(file))
    val parser = new Parser(source, path.toString)
    readEvalPrint(parser, show)
    source.close()
  }

  def readEvalPrint(sourceString: String, show:  Boolean): Unit =
      readEvalPrint(new Parser(io.Source.fromString(sourceString), "<String>"), show)

  def readEvalPrint(parser: Parser, show:  Boolean): Unit = {
    parser.syntaxEnv=syntaxEnv // for JIT compilation of operators
    try {
      while (parser.nextSymb() != Lexical.EOF) try {
        val e = parser.read
        if (show) normalFeedback(s"${e.position}: $e => ")
        val result = try run(e) catch {
          case exn: RuntimeError => exn
          case exn: SyntaxError => exn
        }
        result match {
          case Nothing          =>
          case _ if result==nil =>
          case _                => if (show) normalFeedback(s"$result\n") else errorFeedback(s"${result.show}\n")
        }

      } catch {
        case exn: SyntaxError => errorFeedback(exn.show)
      }
    }
    catch {
      case exn: SyntaxError => errorFeedback(exn.show)
      case exn: Exception   => errorFeedback(exn.toString)
    }
  }



}

