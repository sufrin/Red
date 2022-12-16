package RedScript

import RedScript.Language.{Dot, Pair, SExpSeq, SyntaxError, nil}


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

  case class Num(value: Long) extends Symbol
  case class Hex(value: Long) extends Symbol

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

  def inHex: Boolean = in.ch match {
    case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => true
    case 'a'|'b'|'c'|'d'|'e'|'f' => true
    case 'A'|'B'|'C'|'D'|'E'|'F' => true
    case _ => false
  }

  def inSymbol: Boolean = in.ch match {
     case ')'   => false
     case '('   => false
     case '#'   => false
     case '\n'  => false
     case '"'   => false
     case '\u0000' => false
     case '`'  => false
     case '.'  => false
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

   def nextLongString(close: Char): Lexical.Symbol = {
     val open  = in.ch
     var count = 1
     val start = position
     val buf = new collection.mutable.StringBuilder()
     while (count>0 && getNext()!='\u0000') {
       if (in.ch==open)  {count += 1 }
       else
       if (in.ch==close) { count -= 1 }
       if (count>0) buf.append(in.ch)
     }
     if (in.ch=='\u0000') throw Language.SyntaxError(s"Unclosed long string at $start")
     getNext()
     Str(buf.toString().intern())
   }
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
       case '\u201c' => nextLongString('\u201d')
       case '\u2018' => nextLongString('\u2019')


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

       case '\\' =>
         getNext() match {
           case 'x' | 'X' =>
             val buf = new collection.mutable.StringBuilder()
             while ({getNext(); inHex }) buf.append(in.ch)
             import Useful.CharSequenceOperations._
             Hex(buf.toString().hexToLong.get)

           case other =>
             val buf = new collection.mutable.StringBuilder()
             buf.append("\\")
             while ({getNext(); inSymbol}) buf.append(in.ch)
             Chunk(buf.toString().intern(), symbolic = true)
         }

       case '0'  => //Hexadecimal numbers start with 0x or 0X
         val buf = new collection.mutable.StringBuilder()
         val hex = "Xx".contains(getNext())
         if (hex) {
           while ( { getNext(); inHex }) buf.append(in.ch)
           import Useful.CharSequenceOperations._
           Hex(buf.toString().hexToLong.get)
         } else {
           var n: Long = in.ch-'0'
           while (getNext().isDigit) n = n*10 + (in.ch-'0')
           Num(n)
         }

       case other if other.isDigit =>
         var n: Long = other-'0'
         while (getNext().isDigit) n = n*10 + (in.ch-'0')
         Num(n)

       case '.' =>
         getNext()
         Chunk(".", symbolic = true)

       case other if (other.isLetterOrDigit) =>
         val buf = new collection.mutable.StringBuilder()
         while (in.ch.isLetterOrDigit || in.ch=='\'' || in.ch=='*' || in.ch==':') { buf.append(in.ch); getNext() }
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

   /**
    * Read a sequence of zero or more expressions
    * that follow a `(`:
    *
    * Leaves the input positioned at the ')' that follows
    * the sequence.
    */
   def exprs: List[SExp] = symb match {
     case EOF | EOL  => Nil
     case Ket        => Nil
     case _          => expr :: exprs
   }

  /**
   * Read a sequence of zero or more expressions
   * that follow a `[`:
   *
   * Leaves the input positioned at the ']' that follows
   * the sequence.
   */
  def sqexprs: List[SExp] = symb match {
    case EOF | EOL          => Nil
    case SqKet              => Nil
    case _                  => expr :: sqexprs
  }

  /**
   * Read a sequence of zero or more expressions
   * on a single line.
   *
   * Leaves the input positioned at the `EOL` that follows
   * the sequence.
   */
  def lineOfExprs: List[SExp] = {
    val res =
    symb match {
      case Ket | SqKet => nextSymb(); Nil // TODO: fix inelegance
      case EOF | EOL   => Nil
      case _           => expr :: lineOfExprs
    }
    if (symb!=EOF && symb!=EOL) throw SyntaxError(s"Bad symbol $symb terminates single line of expressions: $res, with positions:  ${res.map(_.position).mkString("«", " ", "»")}")
    res
  }

  /**
   *  Read the body of a parenthesised expression, which can be one of:
   *  {{{
   *    ()
   *    (hd.tl)
   *    (hd)
   *    (hd tl1 tl2 ...)
   *  }}}
   *
   * Leaves the input positioned at the first symbol that follows
   * the body.
   */
  def pairOrExprs: SExp = symb match {
    case Ket        => nextSymb(); nil
    case EOF | EOL  => nil
    case other      => pairOrTail(expr)
  }

  /**
   * After reading  {{{ ( hd }}} reads ==> reduces
   * {{{ . tl ) ==> (hd.tl)}}}
   *
   * or
   * {{{ ) ==> (hd) }}}
   *
   * or
   * {{{ tl1 tl2 ... ) ==> (hd tl1 tl2 ...) }}}
   *
   *  Leaves the input positioned at the first symbol that follows
   * the expression.
   */
  def pairOrTail(hd: SExp): SExp =
    symb match {
      // ( hd .         tl)
      case Chunk("↦", _) | Chunk("=>", _) =>
        val tl  = readWithClosing (Ket) { expr }
        val res = Pair(hd, tl)
        res.position = hd.position
        res
      // (hd            )
      case Ket =>
        nextSymb()
        SExpSeq(List(hd))
      // (hd            e1 ... )
      case other =>
        val tl = exprs
        if (symb==Ket) nextSymb() else throw SyntaxError(s"Expecting ')' after ($hd ... but looking at $symb")
        SExpSeq(hd :: tl)
    }

  /**
   * Read a complete expression.
   *
   * Leaves the input positioned at the first symbol that follows
   * the expression.
   */
  def expr: SExp = {
    val pos = this.position
    val res =
    symb match {
      case Quote => nextSymb(); Language.Quote(expr)
      case Bra   => nextSymb(); pairOrExprs
      case SqBra => SExpSeq(readWithClosing (SqKet) { sqexprs } )
      case Chunk(text, symbolic) => nextSymb()
        syntaxEnv(text) match {
          case None                =>
               val v = Language.Variable (text)
               v.symbolic=symbolic
               v
          case Some(syntacticForm) => syntacticForm
        }
      case Num(value) => nextSymb(); Language.Num(value)
      case Hex(value) => nextSymb(); new Language.Hex(value)
      case Str(text) => nextSymb(); Language.Str(text)
      case other => nextSymb(); Language.Variable(other.toString)
    }
    res.position = pos
    symb match {
      case Chunk(".", _) =>
        val pos = this.position
        nextSymb()
        val d = Dot(res, expr)
        d.position = pos
        d
      case _ => res
    }
  }

  /**
   *  Assumes the input is position (just) to the left of a construct that
   *  can be parsed by `read`, and is expected to terminate with `symb`.
   *  The construct is parsed, and the `symb` skipped if it is
   *  present; otherwise a syntax error is noted.
   */
  def readWithClosing[T](symb: Symbol)(read: => T): T = {
    val start = position
    nextSymb()
    val e = read
    if (this.symb!=symb)
      throw SyntaxError(s"Expecting $symb after expression starting at $start. Seeing ${this.symb} (${position})")
    else {
      nextSymb()
      e
    }
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
      case Quote => expr
      case other => lineOfExprs match {
        case List(e)  => e
        case es       => SExpSeq(es)
      }
    }
    res.position=pos
    res
  }
  

}
