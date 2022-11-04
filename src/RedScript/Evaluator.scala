package RedScript


import java.io.FileInputStream
import java.nio.file.Path
import scala.io.BufferedSource
import scala.language.postfixOps

/**
 *  A global evaluator that defines syntactic forms as well as the semantics of
 *  primitive functions. In this implementation, "abstract syntax" and "semantics" are
 *  intermingled: in the sense that it is S-Expressions that are evaluated. There is no
 *  semantic feature of the language that makes this ''necessary'', but the language
 *  was evolving during its implementation, and this made it straightforward to
 *  introduce new constructs.
 *
 *  TODO: separately define an executable abstract syntax that can be constructed
 *        while parsing: (The `sExps` function of the parser is the right place to
 *        do most of this work; which can proceed (mostly) bottom-up)
 */
class Evaluator {
  import RedScript.Language._

  /**
   * A multi-branched conditional expression takes the form:
   *
   * `(if' (cond . thenExpr)*)` -- zero or more (condition, expr) pairs
   *
   * It yields `Nothing` if no condition yields true; else
   * it yields the value of the `thenExpr` paired with the first
   * of the conditions that yields `true`.
   */
  def evCond(env: Env, body: SExp): SExp = {
    def evCases(cases: List[SExp]): SExp = cases match {
      case Nil => Nothing

      case Pair(pred, thenPart) :: cases =>
        pred.eval(env) match {
          case Bool(true) => thenPart.eval(env)
          case Bool(false) => evCases(cases)
          case other => throw RuntimeError(s"condition evaluates to a non-Bool: $other", Some(pred))
        }

      case List(default) => default.eval(env)

      case other => throw SyntaxError(s"Malformed conditional: ${cases.mkString(" ")}")
    }
    body match {
      case SExps(cases) => evCases(cases)
      case _            => throw SyntaxError(s"Malformed conditional body $body")
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

  // TODO: (decls, body) = pairs.splitAtFirstNonPair
  def evLet(isVar: Boolean)(env: Env, form: SExp): SExp = {
    form match {
      case SExps(pairs) if pairs.length >= 2 =>
        pairs.last match {
          case p: Pair =>
            // A globally-scoped declaration
            // (let (vi . ei )*)
            if (isVar)
              for { Pair(Variable(bv), expr) <- pairs } global.define(bv, Ref(bv, expr.eval(env)))
            else
              for { Pair(Variable(bv), expr) <- pairs } global.define(bv, expr.eval(env))
            Nothing
          // A locally-scoped declaration
          // (let (vi . ei )* body)
          case body =>
            try {
              val decls = pairs.take(pairs.length-1)
              //val bindings = (for {i <- 0 until pairs.length - 1} yield pairs(i)).toList
              val bvs = decls.map { case Pair(bv, _) => bv }
              // val bvs = (for { SExps(List(bv, _)) <- bindings } yield bv).toList
              val args =
                if (isVar)
                  for { Pair(bv, expr)  <- decls } yield Ref(bv.toString, expr.eval(env))
                else
                  for { Pair(bv, expr)  <- decls } yield expr.eval(env)
              body.eval(env.extend(SExps(bvs), args.toList))
            } catch {
              case exn: MatchError => throw SyntaxError(s"Malformed declaration")
            }
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

  def evDef(env: Env, form: SExp): SExp = {
    form match {
      //   def (f . arg) body
      case SExps(Pair(Variable(name),  (allArg: Variable))  :: body) =>
        global.define(name, ExprAll(env, allArg, mkSequential(body)) )
        Nothing
      //   def (f a b c) body
      case SExps(SExps(Variable(name) :: pattern) :: body) if isPattern(pattern) =>
           global.define(name, Expr(env, SExps(pattern), mkSequential(body)))
           Nothing
      case _ => throw SyntaxError(s"Malformed definition $form: should be (def (f . args)  ...) or (def (f arg  ...) ...)")
    }
  }

  def evDefForm(env: Env, form: SExp): SExp = {
    form match {
      //   def (f arg) body
      case SExps(SExps(Variable(name) :: (pattern@Pair(_: Variable, _: Variable)) :: Nil) :: body) =>
        global.define(name, FExprAll(env, pattern, mkSequential(body)) )
        Nothing
      //   def (f a b c) body
      case SExps(SExps(Variable(name) :: pattern) :: body) if isPattern(pattern) =>
        global.define(name, FExpr(env, SExps(pattern), mkSequential(body)))
        Nothing
      case _ => throw SyntaxError(s"Malformed definition: should be (def (f . args)  ...) or (def (f arg  ...) ...)")
    }
  }

  def evMap(env: Env, form: SExp): SExp = form match {
    case SExps(pairs) => EnvExpr(new LocalEnv(pairs.map { case (SExps(List(d, r))) => (d.eval(env).toPlainString, r.eval(env)) }))
  }

  def evFun(env: Env, form: SExp): SExp = {
    form match {
      // fun var body -- binds all params to the variable
      case SExps((all: Variable) :: body) => ExprAll(env, all, mkSequential(body))
      // fun (a b c) body -- binds params to the right number of args
      case SExps(pattern :: body) if isPattern(pattern) => Expr(env, pattern, mkSequential(body))
      // incorrect
      case SExps(pattern :: body) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function expression: $form")
    }
  }

  def evForm(env: Env, form: SExp): SExp = {
    form match {
      // form (env . args) body
      case SExps((pattern: Pair) :: body)  => FExprAll(env, pattern, mkSequential(body))
      // form (a b c) body
      case SExps(pattern :: body) if isPattern(pattern) => FExpr(env, pattern, mkSequential(body))
      // form all body
      case SExps(pattern :: body) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function expression: $form")
    }
  }

  def evPlainString(args: List[SExp]): SExp =  {
    args match {
      case Nil => Str("")
      case List(c) => Str(c.toPlainString)
      case cs =>
        Str(cs.map(_.toPlainString).mkString("[", " ", "]"))
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
    val opn: (SExp,SExp)=>SExp  = { case (a,b) => Str(op(a.toPlainString, b.toPlainString)) }
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
   *  The top-level environment binds, as `SExps`, names subject to "just-in-time"
   *  translation in the parser. When any of the names defined here appear
   *  in an expression they are JIT-translated into their value. This
   *  means that the cost of evaluating a predefined language form, or primitive, is not
   *  dependent on the length of the environment at the time of its evaluation.
   */
  val primitives: List[(String, SExp)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null",       { case List(SExps(Nil)) => Bool(true); case List(_) => Bool(false); case _ => throw SyntaxError("null requires an argument") }),
    "hd"        -> Subr("hd",         { case List(SExps((h::t))) => h; case List(SExps(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl",         { case List(SExps((h::t))) => SExps(t); case List(SExps(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "::"        -> Subr("::",         { case List(s, SExps(ss)) => SExps(s::ss)  }),
    "member"    -> Subr("member",     { case List(s, SExps(ss)) => Bool(ss.contains(s))  }),
    "++"        -> Subr("++",         { case List(SExps(k0), SExps(k1)) => SExps(k0++k1); case other => throw RuntimeError(s"malformed ++: ${SExps(other)}") } ),
    ":="        -> FSubr (":=",       evSet),
    "variable"  -> FSubr ("variable", evGlobal(true)),
    "constant"  -> FSubr ("constant", evGlobal(false)),
    "endsWith"  -> Subr  ("endsWith",   { case List(Str(a), Str(b)) => Bool(a.endsWith(b))}),
    "startsWith"-> Subr  ("startsWith", { case List(Str(a), Str(b)) => Bool(a.startsWith(b))}),
    "matches"   -> Subr  ("matches",    { case List(Str(a), Str(b)) => Bool(a.matches(b))}),
    "contains"  -> Subr  ("contains",   { case List(Str(a), Str(b)) => Bool(a.contains(b))}),
    "var"       -> FSubr ("var",      evLet(true)),
    "val"       -> FSubr ("val",      evLet(false)),
    "if*"       -> FSubr ("if*",      evCond),
    "if"        -> FSubr ("if",       evIf),
    "eval"      -> Subr("eval",       {
      case List(expr) => expr.eval(global)
      case List(EnvExpr(env), expr) => expr.eval(env)
     }),
    "def"       -> FSubr ("def",      evDef),
    "defFun"    -> FSubr ("defFun",   evDef),
    "defForm"   -> FSubr ("defForm",  evDefForm),
    "form"      -> FSubr ("form",     evForm),
    "fun"       -> FSubr ("fun",      evFun),
    "\u03bb"    -> FSubr ("\u03bb",   evFun),  // lambda
    "@"         -> Subr  ("@",        {
      case List(EnvExpr(env), arg) => env.apply(arg.toPlainString) match { case None => Nothing; case Some(value) => value}
    }),
    "map"       -> FSubr  ("map",     evMap),
    "seq"       -> Subr  ("seq",      { case Nil => Nothing; case args => args.last }),
    "readEvalPrint"  -> Subr  ("readEvalPrint", { case Str(text) :: Bool(show) :: rest => readEvalPrint(text, show); Nothing}),
    "println"   -> Subr  ("println",  { case args => args.foreach{ case k => normalFeedback(k.toPlainString); normalFeedback(" ") } ; normalFeedbackLn(""); Nothing }),
    "log"       -> Subr  ("log",      { case args => Logging.Default.log(Logging.INFO, args.map(_.toPlainString).mkString("", " ", "")); Nothing }),
    "?"         -> Subr  ("?",        { case args => args.foreach{ case k => normalFeedback(k.toPlainString); normalFeedback(" ") }; args.last }),
    "toString*" -> Subr  ("toString*", evPlainString),
    "quote"     -> FSubr ("quote",    { case (env, form) => Quote(form) }),
    "list"      -> Subr  ("list",     { args => SExps(args)}),
    "isAtom"    -> forall("isAtom")   { case Quote(Variable(_)) => true; case _ => false },
    "isSymb"    -> forall("isSymb")   { case Quote(v@Variable(_)) => v.symbolic; case _ => false },
    "isVar"     -> forall("isVar")    { case Quote(v@Variable(_)) => !v.symbolic; case _ => false },
    "isNum"     -> forall("isNum")    { case Num(_)=>true;    case _ => false },
    "isList"    -> forall("isList")   { case SExps(_)=>true;  case _ => false },
    "isString"  -> forall("isString") { case Str(_)=>true;    case _ => false },
    "isNothing" -> forall("isNothing"){ case Nothing => true; case _ => false },
    "toString"  -> Subr("toString",   { case List(sexp) => Str(sexp.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "&&"        -> forall("&&")       { case Bool(b)=> b },
    "||"        -> exists("||")       { case Bool(b)=> b },
    "string"    -> redString("string", (_.+(_))),
    "+"         -> red("+",   (_.+(_))),
    "-"         -> minus, // special treatment of unary negation
    "max"       -> red("max", (_.max(_))),
    "min"       -> red("min", (_.min(_))),
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
    try sexp.eval(global) catch {
      case exn: RuntimeError => exn.position = sexp.position; exn
      case exn: SyntaxError  => exn.position = sexp.position; exn
      case exn: scala.Error  => Str(s"Scala Error: ${exn.getMessage} $position")
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
        if (show) normalFeedbackLn(s"${e.position}: $e => «")
        val result = try run(e) catch {
          case exn: RuntimeError => exn
          case exn: SyntaxError => exn
        }
        result match {
          case Nothing          =>
          case _ if result==nil =>
          case _                => if (show) normalFeedbackLn(s"$result »") else normalFeedbackLn(result.toPlainString)
        }

      } catch {
        case exn: SyntaxError => errorFeedback(exn.toPlainString)
      }
    }
    catch {
      case exn: SyntaxError => errorFeedback(exn.toPlainString)
      case exn: Exception   => errorFeedback(exn.toString)
    }
  }



}

