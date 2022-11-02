package RedScript

import RedScript.Language.{Pair, SExps, SyntaxError, Variable}


object Lexical {

  trait Symbol

  case object Bra extends Symbol {
    override def toString = "("
  }

  case object Ket extends Symbol {
    override def toString = ")"
  }

  case object SqBra extends Symbol {
    override def toString = "["
  }

  case object SqKet extends Symbol {
    override def toString = "]"
  }

  //case object True extends Symbol

  //case object False extends Symbol

  case object EOF extends Symbol

  case object EOL extends Symbol

  case object Quote extends Symbol

  case class Chunk(text: String, symbolic: Boolean=false) extends Symbol

  case class Str(text: String) extends Symbol

  case class Num(value: Int) extends Symbol

}

class Parser(source: io.Source, val path: String="") {
  def reset: Parser = new Parser(source)

  private val in = new source.Positioner()

  def position: SourcePosition = lastPosition
  var lastPosition: SourcePosition = SourcePosition(path,1,0)

  var syntaxEnv: Env = null

   private var braCount = 0

   @inline def char: String = in.ch match {
     case '\n' => "'\\n'"
     case ' ' => "'\\s'"
     case ch if ch<' ' => s"'\\${ch.toInt}'"
     case ch => s"'$ch'"
   }
   override def toString: String = s"Parser(${char}@${(in.cline,in.ccol)})"

   locally {
     getNext()
     lastPosition = SourcePosition(path, in.cline, in.ccol-1)
   }

   import Lexical._

   def inSymbol: Boolean = in.ch match {
     case ')'   => false
     case '('   => false
     case '#'   => false
     case '\n'  => false
     case '"'   => false
     case '\u0000' => false
     case '`'  => false
     case other => !other.isSpaceChar & !other.isLetterOrDigit
   }

   def getNext(): Char = {
     if (source.hasNext)
     {  val c =  in.next()
       c
     }
     else
     { in.ch = '\u0000'; in.ch }
   }

   def hasNextSymb: Boolean = in.ch != '\u0000'

   var symb: Lexical.Symbol = EOL

   @inline private def isHexit(c: Char): Boolean =
     ('a'<=c&&c<='f') || ('A'<=c&&c<='F')|| ('0'<=c&&c<='9')

   def nextSymb(): Lexical.Symbol = {
     lastPosition = SourcePosition(path, in.cline, in.ccol-1)
     symb =
     in.ch match {
       case '\u0000' => EOF
       case '`' => getNext(); Quote
       case ')' => braCount = 0 max (braCount-1); getNext(); Ket
       case '(' => braCount+=1; getNext(); Bra
       case ']' => braCount = 0 max (braCount-1); getNext(); SqKet
       case '[' => braCount+=1; getNext(); SqBra
       case '#' =>
         while (getNext()!='\n' && in.ch!='\u0000') {}
         nextSymb()
       case '\n' =>
         while (getNext()=='\n') {}
         if (braCount==0) EOL  else nextSymb()
       case ' ' =>
         while (getNext()==' ') {}
         nextSymb()
       case '\t' =>
         getNext()
         nextSymb()

       // So as to avoid tedious error reports, we have a somewhat philistine approach:
       // * Malformed `\u` escapes are not flagged as errors but treated literally
       // * End-of-file within a string is a (fatal) lexical error
       case '"' =>
         val buf = new collection.mutable.StringBuilder()
         var going = true

         while (going && getNext()!='"' && in.ch!='\u0000') {
           val theChar: Char =
           in.ch match {
             case '\n' => throw Language.SyntaxError(s"Unclosed string: \"${buf.toString}\" $position")
             case '\\' => getNext() match {
               case '\\' => '\\'
               case 'n'  => '\n'
               case 's'  => ' '
               case '\n' => throw Language.SyntaxError(s"Character escape \\ at the end of a line: \"${buf.toString}\" $position")

               case 'u' | 'U' => {
                 import Useful.CharSequenceOperations._
                 var s = s"\\${in.ch}"
                 while (s.length<6 && isHexit(getNext())) s = s + in.ch
                 if (in.ch=='"' || in.ch=='\n') going = false
                 if (s.length==6) {
                   // 6 characters; the last 4 are hexits.
                   s.toUnicode match {
                     case Some(ch) => ch
                     case None     => throw new IllegalStateException(s"\"$s\".toUnicode fails")
                 }
                 } else {
                   buf.append(s)
                   in.ch
                 }
               }
               //'\ch'
               case ch => ch
             }
             // 'ch'
             case ch => ch
           }
           if (going) buf.append(theChar)
         }
         //
         if (in.ch=='\u0000') throw Language.SyntaxError(s"Unclosed string: \"${buf.toString}\" $position") else
         getNext()
         Str(buf.toString().intern())

       case other if other.isDigit =>
         var n: Int = other-'0'
         while (getNext().isDigit) n = n*10 + (in.ch-'0')
         Num(n)

       case other if (other.isLetterOrDigit) =>
         val buf = new collection.mutable.StringBuilder()
         while (in.ch.isLetterOrDigit || in.ch=='\'' || in.ch=='*') { buf.append(in.ch); getNext() }
         Chunk(buf.toString().intern(), symbolic = false)

       case other if inSymbol =>
         val buf = new collection.mutable.StringBuilder()
         buf.append(in.ch)
         while ({getNext(); inSymbol}) buf.append(in.ch)
         Chunk(buf.toString().intern(), symbolic = true)
     }
     symb
   }

    import Language.SExp

   /** Parse the (possibly-empty) body of an SExpr */
   def exprs: List[SExp] = symb match {
     case Ket         => Nil
     case SqKet       => Nil
     case EOF | EOL   => Nil
     case _           => expr :: exprs
   }

  /** Parse the (possibly-empty) body of an `SExprs`; or
   * parse a dotted-pair followed by the (possibly-empty)
   * body of an `SExprs`. A pair takes the form
   * {{{ (e1.e2) }}} but we accept the form
   * {{{ (e1 . e2 e3 ...) }}}
   * and transform it into
   * {{{ (e1 e2 e3 ...) }}}
   * It would be just as easy to consider this a syntax error. See
   * the second case of `sExps`, below.
   */
  def pairOrExprs: List[SExp] = symb match {
    case Ket         => Nil
    case SqKet       => Nil
    case EOF | EOL   => Nil
    case _           => maybePair(expr) :: exprs
  }

  /** `nextSymbol()`; parse using `expr`; check it ends with `symb`, which is to be skipped if so.  */
  def after[T](parse: => T)(symb: Symbol): T = {
      val start = position
      nextSymb()
      val e = parse
      if (this.symb!=symb)
         throw SyntaxError(s"Expecting $symb after expression starting at $start. Seeing ${this.symb} (${position})")
      else {
        nextSymb()
        e
      }
  }

  def maybePair(res: SExp): SExp =
    symb match {
      case Chunk(".", _) =>
        nextSymb()
        val res_ = Language.Pair(res, expr)
        res_.position = res.position
        res_
      case _ => res
    }

  /**
   *  (p.q r s t u) ==> (p . (seq q r s t u))
   */
  def sExps(exprs: List[SExp]): SExp = exprs match {
    case List(p: Pair)  => p
    case (Pair(l, r)) :: rest => Pair(l, SExps(Variable("seq")::r::rest))
    case _ => SExps(exprs)
  }

  def expr: SExp = {
    val pos = this.position
    val res =
    symb match {
      case Quote => nextSymb(); Language.Quote(expr)
      case Bra   =>  sExps(after { pairOrExprs } (Ket))
      case SqBra =>  sExps(after { exprs } (SqKet))
      case Chunk(text, symbolic) => nextSymb()
        syntaxEnv(text) match {
          case None                =>
               val v = Language.Variable (text)
               v.symbolic=symbolic
               v
          case Some(syntacticForm) => syntacticForm
        }
      case Num(value) => nextSymb(); Language.Num(value)
      case Str(text) => nextSymb(); Language.Str(text)
      case other => nextSymb(); Language.Variable(other.toString)
    }
    res.position = pos
    res
  }

  /** Read a top-level expression, viz:
   *
   *   A composite, properly bracketed, s-expression starting with an
   *   opening bracket and terminated by an "unprotected" EOL/EOF. The following
   *   are equivalent:
   *   {{{
   *     (foo baz)
   *     ( foo
   *       baz
   *     )
   *   }}}
   *
   *   a single primitive expression: variable, symbol, string, number
   *   {{{
   *     foo
   *     "foo"
   *     1234
   *     -->
   *   }}}
   *
   *   A composite, properly bracketed, s-expression starting with a
   *   primitive expression and terminated by an "unprotected" EOL/EOF.
   *   The following are equivalent:
   *   {{{
   *     list `("this" "machine" "kills")
   *     list `("this"
   *     "machine"
   *     "kills")
   *   }}}
   */
  def read: SExp = {
    while (symb==EOL) nextSymb()
    val pos = position
    val res: SExp =
    symb match {
      case Bra   => expr
      case SqBra => expr
      case other => exprs match {
        case List(e)     => e
        case application => Language.SExps(application)
      }
    }
    res.position=pos
    res
  }
  

}
