package RedScript

import RedScript.Language.SyntaxError


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

  case object True extends Symbol

  case object False extends Symbol

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
  var lastPosition: SourcePosition = SourcePosition(path,-1,-1)

   private var braCount = 0

   @inline def char: String = in.ch match {
     case '\n' => "'\\n'"
     case ' ' => "'\\s'"
     case ch if ch<' ' => s"'\\${ch.toInt}'"
     case ch => s"'$ch'"
   }
   override def toString: String = s"Parser(${char}@${(in.cline,in.ccol)})"

   locally { getNext() }

   import Lexical._

   def inSymbol: Boolean = in.ch match {
     case ')'   => false
     case '('   => false
     case '#'   => false
     case '\n'  => false
     case '"'   => false
     case '\u0000' => false
     case '\''  => false
     case other => !other.isSpaceChar & !other.isLetterOrDigit
   }

   def getNext(): Char = {
     if (source.hasNext) in.next() else { in.ch = '\u0000'; in.ch }
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
       case '\'' => getNext(); Quote
       case ')' => braCount = 0 max (braCount-1); getNext(); Ket
       case '(' => braCount+=1; getNext(); Bra
       case ']' => braCount = 0 max (braCount-1); getNext(); SqKet
       case '[' => braCount+=1; getNext(); SqBra
       case '#' =>
         while (getNext()!='\n') {}
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

       // So as to avoid tedious error reports, we have a philistine approach:
       // Newlines are accepted within strings
       // Malformed `\u` escapes are not flagged as errors but treated literally
       // End-of-file within a string is a (fatal) lexical error
       case '"' =>
         val buf = new collection.mutable.StringBuilder()
         var going = true

         while (going && getNext()!='"' && in.ch!='\u0000') {
           val theChar: Char =
           in.ch match {
             case '\n' => throw Language.SyntaxError(s"Unclosed string: \"${buf.toString}\" $position")
             case '\\' => getNext() match {
               case '\\' => getNext(); '\\'
               case 'n' => getNext(); '\n'
               case 's' => getNext(); ' '
               case '\n' => throw Language.SyntaxError(s"Character escape \\ at the end of a line: \"${buf.toString}\" $position")

               case 'u' | 'U' => {
                 import Useful.CharSequenceOperations._
                 var s = s"\\${in.ch}"
                 while (s.length<6 && isHexit(getNext())) s = s + in.ch
                 if (in.ch=='"' || in.ch=='\n') going = false
                 if (s.length==6) {
                   // 6 hex digits
                   s.toUnicode match {
                     case Some(ch) => ch
                     case None     => throw new IllegalStateException(s"\"$s\".toUnicode fails")
                 }
                 } else  {
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
           buf.append(theChar)
         }
         //
         if (in.ch=='\u0000') throw Language.SyntaxError(s"Unclosed string: \"${buf.toString}\" $position") else
         getNext()
         Str(buf.toString())

       case other if other.isDigit =>
         var n: Int = other-'0'
         while (getNext().isDigit) n = n*10 + (in.ch-'0')
         Num(n)

       case other if (other.isLetterOrDigit) =>
         val buf = new collection.mutable.StringBuilder()
         while (in.ch.isLetterOrDigit) { buf.append(in.ch); getNext() }
         buf.toString match {
           case "true"  => True
           case "false" => False
           case text    => Chunk(text, symbolic = false)
         }

       case other if inSymbol =>
         val buf = new collection.mutable.StringBuilder()
         buf.append(in.ch)
         while ({getNext(); inSymbol}) buf.append(in.ch)
         Chunk(buf.toString(), symbolic = true)
     }
     symb
   }

    import Language.SExp

   def exprs: List[SExp] = symb match {
     case Ket         => Nil
     case SqKet       => Nil
     case EOF | EOL   => Nil
     case _           => expr :: exprs
   }

  /** `nextSymbol()`; parse using `expr`; check it ends with `symb`, which is to be skipped if so.  */
  def after(expr: => List[SExp])(symb: Symbol): List[SExp] = {
      val start = position
      nextSymb()
      val e = expr
      if (this.symb!=symb)
         throw SyntaxError(s"Expecting $symb after expression starting at $start. Seeing ${this.symb} (${position})")
      else {
        nextSymb()
        e
      }
  }

  def expr: SExp = {
    val pos = this.position
    val res =
    symb match {
      case Quote => nextSymb(); Language.Quote(expr)
      case Bra   =>  Language.SExps(after { exprs } (Ket))
      case SqBra =>  Language.SExps(after { exprs } (SqKet))
      case Chunk(text, symbolic) => nextSymb(); (if (symbolic) Language.Symbol else Language.Variable)(text)
      case Num(value) => nextSymb(); Language.Num(value)
      case Str(text) => nextSymb(); Language.Str(text)
      case True => nextSymb(); Language.Bool(true)
      case False => nextSymb(); Language.Bool(false)
      case other => nextSymb(); Language.Variable(other.toString)
    }
    res.position = pos
    res
  }

  /** Read a top-level expression: it can consist of a non-bracketed line */
  def read: SExp = {
    while (symb==EOL) nextSymb()
    symb match {
      case Bra   => expr
      case SqBra => expr
      case other => exprs match {
        case List(e)     => e
        case application => Language.SExps(application)
      }
    }
  }
  

}
