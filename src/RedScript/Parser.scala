package RedScript


object Lexical {

  trait Symbol {
  }

  case object Bra extends Symbol {
    override def toString = "("
  }

  case object Ket extends Symbol {
    override def toString = ")"
  }

  case object True extends Symbol {
    override def toString = "true"
  }

  case object False extends Symbol {
    override def toString = "false"
  }

  case object EOF extends Symbol

  case object EOL extends Symbol

  case object Quote extends Symbol {
    override def toString = "'"
  }



  case class Atom(text: String) extends Symbol {
    override def toString = text
  }

  case class Str(text: String) extends Symbol {
    override def toString = text
  }

  case class Num(value: Int) extends Symbol {
    override def toString = value.toString
  }

}

class Parser(source: io.Source, val path: String="") {
  def reset: Parser = new Parser(source)

  private val in = new source.Positioner()

  def position: SourcePosition = lastPosition
  var lastPosition: SourcePosition = new SourcePosition((path,-1,-1))

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

   def inAtom: Boolean = in.ch match {
     case ')'   => false
     case '('   => false
     case '#'   => false
     case '\n'  => false
     case '"'   => false
     case '\u0000' => false
     case '\''  => false
     case other => !other.isSpaceChar
   }

   def getNext(): Char = {
     if (source.hasNext) in.next() else { in.ch = '\u0000'; in.ch }
   }

   def hasNextSymb: Boolean = in.ch != '\u0000'

   var symb: Lexical.Symbol = EOL

   @inline private def isHexit(c: Char): Boolean =
     ('a'<=c&&c<='f') || ('A'<=c&&c<='F')|| ('0'<=c&&c<='9')

   def nextSymb(): Lexical.Symbol = {
     lastPosition = new SourcePosition((path, in.cline, in.ccol-1))
     symb =
     in.ch match {
       case '\u0000' => EOF
       case '\'' => getNext(); Quote
       case ')' => braCount = 0 max (braCount-1); getNext(); Ket
       case '(' => braCount+=1; getNext(); Bra
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
             case '\\' => getNext() match {
               case '\\' => getNext(); '\\'
               case 'n' => getNext(); '\n'
               case 's' => getNext(); ' '
               case 'u' | 'U' => {
                 import Useful.CharSequenceOperations._
                 var s = s"\\${in.ch}"
                 while (s.length<6 && isHexit(getNext())) s = s + in.ch
                 if (in.ch=='"') going = false
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
         if (in.ch=='\u0000') throw Syntax.SyntaxError(s"Unclosed string: \"${buf.toString}\" $position") else
         getNext()
         Str(buf.toString())

       case other if other.isDigit =>
         var n: Int = other-'0'
         while (getNext().isDigit) n = n*10 + (in.ch-'0')
         Num(n)

       case other if inAtom =>
         val buf = new collection.mutable.StringBuilder()
         buf.append(in.ch)
         while ({getNext(); inAtom}) buf.append(in.ch)
         buf.toString match {
           case "true"  => True
           case "false" => False
           case text    => Atom(text)
         }
     }
     symb
   }

    import Syntax.SExp

   def exprs: List[SExp] = symb match {
     case Ket         => nextSymb(); Nil
     case EOF | EOL   => Nil
     case _           => expr :: exprs
   }

  def expr: SExp = {
    val pos = this.position
    val res =
    symb match {
      case Quote => nextSymb(); Syntax.Quote(expr)
      case Bra =>  nextSymb(); Syntax.SExps(exprs)
      case Atom(text) => nextSymb(); Syntax.Atom(text)
      case Num(value) => nextSymb(); Syntax.Num(value)
      case Str(text) => nextSymb(); Syntax.Str(text)
      case True => nextSymb(); Syntax.Bool(true)
      case False => nextSymb(); Syntax.Bool(false)
      case other => nextSymb(); Syntax.Atom(other.toString)
    }
    res.position = pos
    res
  }

  /** Read a top-level expression: it can consist of a non-bracketed line */
  def read: SExp = {
    while (symb==EOL) nextSymb()
    symb match {
      case Bra   => expr
      case other => exprs match {
        case List(e)     => e
        case application => Syntax.SExps(application)
      }
    }
  }
  

}
