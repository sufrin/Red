package RedScript

import java.io.FileInputStream
import java.nio.file.Path
import scala.annotation.{nowarn, tailrec}
import scala.io.BufferedSource
import scala.language.postfixOps
@nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
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
   * `(if* (condition . expr)*)`
   *
   * It yields `Nothing` if no condition yields true; else
   * it yields the value of the `expr` paired with the first
   * of the conditions that yields `true`.
   */
  private def evCond(env: Env, body: SExp): SExp = {
    @tailrec
    def evCases(cases: List[SExp]): SExp = cases match {
      case Nil => Nothing

      case Pair(pred, thenPart) :: cases =>
        pred.eval(env) match {
          case Bool(true) => thenPart.eval(env)
          case Bool(false) => evCases(cases)
          case other => throw RuntimeError(s"condition evaluates to a non-Bool: $other", Some(pred))
        }

      case List(default) => default.eval(env)

      case _ => throw SyntaxError(s"Malformed conditional: ${cases.mkString(" ")}")
    }
    body match {
      case SExpSeq(cases) => evCases(cases)
      case _            => throw SyntaxError(s"Malformed conditional body $body")
    }
  }

  /** A single-branched conditional expression takes the form:
   *
   *  `(if condition thenExpr elseExpr)`
   *
   *  It yields the same as
   *
   *  `(if* (condition . thenExpr) (true . elseExpr))`
   */
  private def evIf(env: Env, body: SExp): SExp = body match {
    case SExpSeq(List(pred, thenPart, elsePart)) => pred.eval(env) match {
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
   * Built-in unchangeable forms
   */
  val syntaxEnv = new MutableEnv

  /** Check the declareability of a name */
  private def whenRedefinable[T](sexp: SExp)(expr: => T): T = {
    sexp match {
      case Variable(_) => expr
      case other => throw SyntaxError(s"Built-ins cannot be rebound or redefined: $other")
    }
  }

  private def evSet(env: Env, body: SExp): SExp = {
    body match {
      case SExpSeq(List(lvalue, rvalue)) =>
        lvalue.lval(env).value = rvalue.eval(env)
        Nothing
      case other => throw SyntaxError(s"Malformed assignment: $other")
    }
  }

  // TODO: generalise to multiple declarations
  private def evGlobal(isVar: Boolean)(env: Env, body: SExp): SExp = {
    body match {
      case SExpSeq(List(Variable(name), value)) =>
          val v = value.eval(env)
          global.define(name, if (isVar) Ref(name, v) else v)
          Nothing
      case SExpSeq(List(sexp, _)) => whenRedefinable(sexp) { Nothing } // for consistency of the error message
      case _                    => throw SyntaxError(s"Malformed ${if (isVar) "global" else "constant"} declaration: $body")
    }
  }

  /**
   *  Implements val and var declarations:
   *  `(val (vi => ei )* expr...)`
   *  `(var (vi => ei )* expr...)`
   *
   *  When `expr...` is nonempty the declarations are local to the sequential evalution of the exprs
   *  otherwise they are global.
   */
  private def evLet(isVar: Boolean)(env: Env, form: SExp): SExp = {
    form match {
      case SExpSeq(seq) =>
        val body = seq.dropWhile  { case p: Pair => true; case _ => false }
        val pairs = seq.takeWhile { case p: Pair => true; case _ => false }
            if (body.isEmpty) {
              // A globally-scoped declaration
              // (let (vi => ei )*)
              if (isVar)
                for {Pair(bv, expr) <- pairs} whenRedefinable(bv) {
                  global.define(bv.toString, Ref(bv.toString, expr.eval(env)))
                }
              else
                for {Pair(bv, expr) <- pairs} whenRedefinable(bv) {
                  global.define(bv.toString, expr.eval(env))
                }
              Nothing
            } else {
          // A locally-scoped declaration
          // (let (vi => ei )* body)
            try {
              val bvs   = pairs.map { case Pair(bv, _) => bv }
              val args =
                if (isVar)
                  for { Pair(bv, expr)  <- pairs } yield whenRedefinable(bv) { Ref(bv.toString, expr.eval(env)) }
                else
                  for { Pair(bv, expr)  <- pairs } yield whenRedefinable(bv) { expr.eval(env) }
              mkSequential(body).eval(env.extend(SExpSeq(bvs), args))
            } catch {
              case exn: MatchError => throw SyntaxError(s"Malformed declaration: $form (${exn.getMessage()})")
            }
        }

      case _ => throw SyntaxError(s"Malformed declaration: there should be at least one (variable.value)")
    }
  }

  @inline private def isAtom(pattern: SExp): Boolean = pattern match { case Variable(_) => true ; case _ => false }
  @inline private def isPattern(pattern: List[SExp]): Boolean = pattern.forall(isPattern)
  @inline private def isPattern(pattern: SExp): Boolean = pattern match {
    case SExpSeq(params) if params.forall(isAtom) => true
    case _             if isAtom(pattern)       => true
    case _                                      => false
  }

  private def evDef(env: Env, form: SExp): SExp = {
    form match {
      //   def (f . arg) body // arg bound to the list of actual parameters
      case SExpSeq(Pair(Variable(name),  allArg: Variable)  :: body) =>
           global.define(name, ExprAll(env, allArg, mkSequential(body)) )
           Nothing
      //   def (f arg1 arg2 ...) body // arg1 arg2 ... bound to the actual parameters
      case SExpSeq(SExpSeq(Variable(name) :: pattern) :: body) if isPattern(pattern) =>
           global.define(name, Expr(env, SExpSeq(pattern), mkSequential(body)))
           Nothing
      case _ => throw SyntaxError(s"Malformed definition $form: should be (def (f . args)  ...) or (def (f arg  ...) ...)")
    }
  }

  private def evDefForm(env: Env, form: SExp): SExp = {
    form match {
      //   defForm (f env arg1 ...) body    // env bound to the calling environment, arg1 arg2 ... bound to the actual parameters
      //   defForm (f (env.arg) body        // env bound to the calling environment, arg bound to the list of actual parameters
      case SExpSeq(SExpSeq(Variable(name) :: (pattern@Pair(_: Variable, _: Variable)) :: Nil) :: body) =>
        global.define(name, FExprAll(env, pattern, mkSequential(body)) )
        Nothing
      case SExpSeq(SExpSeq(Variable(name) :: pattern) :: body) if isPattern(pattern) =>
        global.define(name, FExpr(env, SExpSeq(pattern), mkSequential(body)))
        Nothing
      case _ => throw SyntaxError(s"Malformed definition: should be (def (f . args)  ...) or (def (f arg  ...) ...)")
    }
  }

  /**
   * `(map (name.value)* )`
   */
  private def evMap(env: Env, form: SExp): SExp = form match {
    case SExpSeq(pairs) => EnvExpr(new LocalEnv(pairs.map { case Pair(d,r) => (d.eval(env).toPlainString, r.eval(env)) }))
  }

  /** Evaluate a by-value function expression (to a closure)
   *
   * TODO: check redefinability of parameter names
   */
  private def evFun(env: Env, form: SExp): SExp = {
    form match {
      // fun arg body -- binds list of actual params to arg
      case SExpSeq((all: Variable) :: body) => ExprAll(env, all, mkSequential(body))
      // fun (arg1 arg2 ...) body -- binds actual params to arg1 arg2 ....
      case SExpSeq(pattern :: body) if isPattern(pattern) => Expr(env, pattern, mkSequential(body))
      // incorrect
      case SExpSeq(pattern :: _) if !isPattern(pattern) => throw SyntaxError(s"Malformed fun parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function expression: $form")
    }
  }

  /** Evaluate a by-syntax function expression (to a closure)
   *  (see also defForm)
   *  TODO: check redefinability of parameter names
   */
  private def evForm(env: Env, form: SExp): SExp = {
    form match {
      // form (env . args) body         // env bound to calling environment, args bound to list of actual parameters
      case SExpSeq((pattern: Pair) :: body)  => FExprAll(env, pattern, mkSequential(body))
      // form (env arg1 arg2 ...) body  // env bound to calling environment, arg1 arg2 ... bound to actual parameters
      case SExpSeq(pattern :: body) if isPattern(pattern) => FExpr(env, pattern, mkSequential(body))
      // form all body
      case SExpSeq(pattern :: _) if !isPattern(pattern) => throw SyntaxError(s"Malformed parameter(s): ${pattern.position}")
      case _ => throw SyntaxError(s"Malformed function expression: $form")
    }
  }

  private def evPlainString(args: List[SExp]): SExp =  {
    args match {
      case Nil => Str("")
      case List(c) => Str(c.toPlainString)
      case cs =>
        Str(cs.map(_.toPlainString).mkString("[", " ", "]"))
    }
  }

  private def mkAtom(name: String, pos: SourcePosition): Variable = {
    val atom = Variable(name)
    atom.position=pos
    atom
  }

  /**  Workhorse to implement the transformation (for `N>1`)
   *   of a sequence of expressions of length `N` to a single
   *   `(seq ...)` expression. Used to systematically transform
   *   the `body` part of a function expression or definition.
   */
  @inline private def mkSequential(exprs: List[SExp]): SExp = exprs match {
    case List(expr) => expr
    case exprs => SExpSeq(syntaxEnv("seq").get :: exprs)
  }


  /** Lazy conjunction of `test` applied to the value of each argument */
  private def forall(name: String)(test: SExp => Boolean): SExp =
    FSubr(name, { case (env, SExpSeq(exprs)) => Bool(exprs.forall(expr => test(expr.eval(env))))})

  /** Lazy disjunction of `test` applied to the value of each argument */
  private def exists(name: String)(test: SExp => Boolean): SExp =
    FSubr(name, { case (env, SExpSeq(exprs)) => Bool(exprs.exists(expr => test(expr.eval(env))))})

  /** Application of `op` to the arguments as a whole */
  private def fun(name: String, op: List[SExp]=>SExp): Subr  =  Subr(name, op)

  /** `op`-(left)reduction of the arguments as a whole */
  private def leftReduceNum(name: String, op: (Long,Long)=>Long):Subr = {
    val opn: (SExp,SExp)=>SExp  = {
      case (a: Hex, b: Num) => new Hex(op(a.value, b.value))
      case (a: Num, b: Hex) => new Hex(op(a.value, b.value))
      case (Num(a),Num(b)) => Num(op(a,b))
    }
    fun(name, { case args: List[SExp] if args.nonEmpty => args.reduceLeft(opn(_,_)) })
  }

  /** `op`-(left)reduction of the arguments as a whole */
  private def leftReduceString(name: String, op: (String,String)=>String):Subr = {
    val opn: (SExp,SExp)=>SExp  = { case (a,b) => Str(op(a.toPlainString, b.toPlainString)) }
    fun(name, { case args: List[SExp] if args.nonEmpty => args.reduceLeft(opn(_,_)) })
  }

  /**
   * `op`-(left)reduction of the arguments as a whole; unless there is a single argument, in which case `op(z, arg)`
   *
   * This permits unary negation straightforwardly
   * Thus: (- x) means negated x, and (- x1 x2 ...) means (x1-x2)-x3)-...
   */
  private def subtractReduce: Subr = {
    val opn: (SExp,SExp)=>SExp  = {
      case (a: Hex, b: Num) => new Hex(a.value -  b.value)
      case (a: Num, b: Hex) => new Hex(a.value - b.value)
      case (Num(a),Num(b)) => Num(a-b)
    }
    fun("-", { case List(n: Hex) => new Hex(-n.value)
               case List(n: Num) => Num(-n.value)
               case args: List[SExp] => args.reduceLeft(opn(_,_)) }) }

  private def binaryRelation(name: String, op: (SExp, SExp) => Boolean):Subr =
    fun(name, { case List(a: SExp, b: SExp) => Bool(op(a,b)) })

  /** `(ENV "var" default)` => value of "var" in the OS environment  */
  private def evalENV(args: List[SExp]): SExp = args match {
    case List(Str(variable), Str(default)) => Str(sys.env.getOrElse(variable, default))
    case List(Str(variable))               => Str(sys.env.getOrElse(variable, ""))
  }

  /** `(PROP "var" default)` => value of "var" as Java/Scala system runtime property  */
  private def evalPROP(args: List[SExp]): SExp = args match {
    case List(Str(variable), Str(default)) => Str(sys.props.getOrElse(variable, default))
    case List(Str(variable))               => Str(sys.props.getOrElse(variable, ""))
  }

  private var position: SourcePosition = SourcePosition("",0,0)

  import RedScript.RedObject._

  /**
   *  The top-level environment binds, as `SExpSeq`, names subject to "just-in-time"
   *  translation in the parser. When any of the names defined here appear
   *  in an expression they are JIT-translated into their value. This
   *  means that the cost of evaluating a predefined language form, or primitive, is not
   *  dependent on the length of the environment at the time of its evaluation.
   */
  private val primitives: List[(String, SExp)] = List(
    "nil"       -> nil,
    "null"      -> Subr("null",       { case List(SExpSeq(Nil)) => Bool(true); case List(_) => Bool(false); case _ => throw SyntaxError("null requires an argument") }),
    "hd"        -> Subr("hd",         { case List(SExpSeq(h::t)) => h; case List(SExpSeq(Nil)) => throw RuntimeError(s"(hd nil)"); case other => RuntimeError("Non-list: (hd $other)") } ),
    "tl"        -> Subr("tl",         { case List(SExpSeq(h::t)) => SExpSeq(t); case List(SExpSeq(Nil)) => throw RuntimeError(s"(tl nil)"); case other => RuntimeError("Non-list: (tl $other)")  } ),
    "::"        -> Subr("::",         { case List(s, SExpSeq(ss)) => SExpSeq(s::ss)  }),
    "member"    -> Subr("member",     { case List(s, SExpSeq(ss)) => Bool(ss.contains(s))  }),
    "++"        -> Subr("++",         { case List(SExpSeq(k0), SExpSeq(k1)) => SExpSeq(k0++k1); case other => throw RuntimeError(s"malformed ++: ${SExpSeq(other)}") } ),
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
    "readEvalPrint" -> Subr  ("readEvalPrint", { case Str(text) :: Bool(show) :: _ => readEvalPrint(text, show, false); Nothing}),
    "map:empty" -> EnvExpr  (new LocalEnv(Nil)),
    "map:"      -> FSubr    ("map:",       evMap),
    "map:add"   -> MapMethods("add"),
    "seq"       -> Subr     ("seq",       { case Nil => Nothing; case args => args.last }),
    "println"   -> Subr     ("println",   { args => args.foreach { k => normalFeedback(k.toPlainString); normalFeedback(" ") }; normalFeedbackLn(""); Nothing }),
    "log"       -> Subr     ("log",       { args => Logging.Default.log(Logging.INFO, args.map(_.toPlainString).mkString("", " ", "")); Nothing }),
    "?"         -> Subr     ("?",         { args => args.foreach{  k => normalFeedback(k.toPlainString); normalFeedback(" ") }; args.last }),
    "toString*" -> Subr     ("toString*", evPlainString),
    "quote"     -> FSubr    ("quote",     { case (env, form) => form }),
    "type"      -> Subr("type", {
      case List(arg) => arg match {
           case _: Variable => Str("ATOM")
           case _: Num => Str("NUM")
           case _: SExpSeq => Str("LIST")
           case _: Str     => Str("STRING")
           case _: Expr    => Str("FUN")
           case _: FExpr   => Str("FEXPR")
           case _: FExprAll => Str("FEXPRALL")
           case _: Subr     => Str("SUBR")
           case obj: Obj   => Str(obj.getType)
           case other      => Str(other.getClass.toString)
     }}),
    "isDefined" -> FSubr("isDefined", {
      case (env, SExpSeq(exps)) =>
        Bool(exps.forall {
          case Variable(name) => env(name).nonEmpty
          case _ : Const => true
        })
    }),
    "toString"  -> Subr("toString",   { case List(sexp) => Str(sexp.toString);  case other => throw RuntimeError(s"malformed toString: $other") }),
    "toNum"     -> Subr("toNum",      { case List(a: Hex) => Num(a.value); case List(n) => n}),
    "toHex"     -> Subr("toHex",      { case List(a: Hex) => a; case List(n: Num) => new Hex(n.value)}),
    "&&"        -> forall("&&")       { case Bool(b)=> b },
    "||"        -> exists("||")       { case Bool(b)=> b },
    "string"    -> leftReduceString("string", _.+(_)),
    "+"         -> leftReduceNum("+",   _.+(_)),
    "-"         -> subtractReduce, // special treatment of unary negation
    "max"       -> leftReduceNum("max", _.max(_)),
    "min"       -> leftReduceNum("min", _.min(_)),
    "*"         -> leftReduceNum("*",   _.*(_)),
    "/"         -> leftReduceNum("/",   _./(_)),
    "&"         -> leftReduceNum("&",   _.&(_)),
    "|"         -> leftReduceNum("|",   _.|(_)),
    "="         -> binaryRelation("=",   _.equals(_)),
    "<"         -> binaryRelation("<",   { case (Num(a), Num(b))=>a<b;  case (Str(a), Str(b))=>a<b;  case (Bool(a), Bool(b))=>a<b  }),
    "<="        -> binaryRelation("<=",  { case (Num(a), Num(b))=>a<=b; case (Str(a), Str(b))=>a<=b; case (Bool(a), Bool(b))=>a<=b }),
    ">"         -> binaryRelation("<",   { case (Num(a), Num(b))=>a>b;  case (Str(a), Str(b))=>a<b;  case (Bool(a), Bool(b))=>a>b  }),
    ">="        -> binaryRelation("<=",  { case (Num(a), Num(b))=>a>=b; case (Str(a), Str(b))=>a<=b; case (Bool(a), Bool(b))=>a>=b }),
    "true"      -> Bool(true),
    "false"     -> Bool(false),
    "ENV"       -> Subr("ENV",   evalENV),
    "PROP"      -> Subr("PROP",  evalPROP),
    "SOURCE"    -> PositionSubr("SOURCE",  { case List(s: Str) => s }), // special case

    // String methods as functions
    "string:cat"   -> StrMethods("cat"),
    "string:range" -> StrMethods("range"),
    // List methods as functions
    "list"       -> Subr  ("list", { args => SExpSeq(args)}),
    "list:range" -> SexpSeqMethods("range"),
    "list:cat"   -> SexpSeqMethods("cat"),
    "list:map"   -> SexpSeqMethods("map"),
    "list:fst"   -> SexpSeqMethods("fst"),
    "list:snd"   -> SexpSeqMethods("snd"),
    "list:foldl" -> SexpSeqMethods("foldl"),
    "list:foldr" -> SexpSeqMethods("foldr"),

    "pair:fst"   -> PairMethods("fst"),
    "pair:snd"   -> PairMethods("snd"),
    /* re:regex has a variety of parameter forms, yielding a variety of regex results
     *     PARAMETER            RESULT
     *     string               non-literal regex
     *     false string         non-literal regex
     *     true  string         literal regex
     *
     *     (list regexes)       branched regex (from regexes
     *     (list strings)       branched regex (from strings compiled as non-literal regexes
     *     false (list strings) branched regex (from strings compiled as non-literal regexes)
     *     true (list strings)  branched regex (from strings compiled as literal regexes)
     *
     *     Branched regexes have a replace method, whose parameters are:
     *
     *     (list template)        rewrites each recognised branch using the appropriate template
     *     (list template) false  rewrites each recognised branch using the appropriate template
     *     (list template) true   rewrites each recognised branch using the appropriate template as a literal
     */
    "re:new"      -> RegexMethods("new"),
    "re:match"    -> RegexMethods("match"),
    "re:span"     -> RegMatchMethods("span"),
    "re:subst"    -> RegMatchMethods("subst"),
    "re:group"    -> RegMatchMethods("group"),
    "re:groups"   -> RegMatchMethods("groups"),
    "re:find"     -> RegexMethods("find"),
    "re:replace"  -> RegexMethods("replace"),

    "queue:new" -> QueueMethods("new"),
    "queue:enq" -> QueueMethods("enq"),
    "queue:deq" -> QueueMethods("deq"),
    "queue:size" -> QueueMethods("size"),
    "queue:toList" -> QueueMethods("toList"),

    "table:new" -> TableMethods("new"),
    "table:eval" -> TableMethods("eval"),
    "table:keys" -> TableMethods("keys"),
    "table:add" -> TableMethods("add"),

  )

  val globals: List[(String, SExp)] = List (
  )

  def run(sexp: SExp): SExp = {
    try sexp.eval(global) catch {
      case exn: RuntimeError => exn.position = sexp.position; exn
      case exn: SyntaxError  => exn.position = sexp.position; exn
      case exn: scala.Error  => RuntimeError(s"Scala Error: ${exn.getMessage} $position")
    }
  }

  for { (name, value) <- primitives } syntaxEnv.define(name, value)
  for { (name, value) <- globals }    global.define(name, value)


  def normalFeedback(s: String): Unit   = { Console.print(s); Console.flush() }
  def normalFeedbackLn(s: String): Unit = { Console.println(s); Console.flush() }
  def errorFeedback(s: String): Unit    = { Console.println(s); Console.flush() }

  def readEvalPrint(path: Path, show:  Boolean, throwError: Boolean): Unit = {
    val file   = path.toFile
    val source = new BufferedSource(new FileInputStream(file))
    val parser = new Parser(source, path.toString)
    readEvalPrint(parser, show, throwError)
    source.close()
  }

  def readEvalPrint(sourceString: String, show:  Boolean, throwError: Boolean): Unit =
      readEvalPrint(new Parser(io.Source.fromString(sourceString), "<String>"), show, throwError)

  def readEvalPrint(parser: Parser, show:  Boolean, throwError: Boolean): Unit = {
    parser.syntaxEnv=syntaxEnv // for JIT compilation of operators
    try {
      while (parser.nextSymb() != Lexical.EOF) try {
        val e = parser.read
        if (show) normalFeedbackLn(s"${e.position}: $e => «")
        val result = run(e)
        result match {
          case Nothing          =>
          case _ if result==nil =>
          case SyntaxError(message, reason) => errorFeedback(message)
          case RuntimeError(message, reason, level) => errorFeedback(message)
          case _                => if (show) normalFeedbackLn(s"$result »") else normalFeedbackLn(result.toPlainString)
        }

      } catch {
        case exn: SyntaxError => errorFeedback(exn.toPlainString); throw exn
      }
    }
    catch {
      case exn: SyntaxError  => if (throwError) throw exn else errorFeedback(exn.toPlainString)
      case exn: RuntimeError => if (throwError) throw exn else errorFeedback(exn.toPlainString)
      case exn: Throwable   => if (throwError) throw exn else errorFeedback(exn.toString)
    }
  }



}

