package RedLisp


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

class Parser(source: io.Source) {
   private val in = new source.Positioner()
   private var braCount = 0

   locally { getNext() }

   import Lexical._

   def inAtom: Boolean = in.ch match {
     case ')'   => false
     case '('   => false
     case '#'   => false
     case '\n'  => false
     case '"'   => false
     case '\u0000' => false
     case other => !other.isSpaceChar
   }

   def getNext(): Char = {
     if (source.hasNext) in.next() else { in.ch = '\u0000'; in.ch }
   }

   def hasNextSymb: Boolean = in.ch != '\u0000'

   var symb: Lexical.Symbol = _

   def nextSymb(): Lexical.Symbol = {
     symb =
     in.ch match {
       case '\u0000' => EOF
       case ')' => braCount-=1; getNext(); Ket
       case '(' => braCount+=1; getNext(); Bra
       case '#' =>
         while (getNext()!='\n') {}
         nextSymb()
       case '\n' =>
         while (getNext()=='\n') {}
         if (braCount==0) { getNext(); EOL } else nextSymb()
       case ' ' =>
         while (getNext()==' ') {}
         nextSymb()
       case '\t' =>
         getNext()
         nextSymb()
       case '"' =>
         val buf = new collection.mutable.StringBuilder()
         while (getNext()!='"') {
           buf.append(in.ch)
         }
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
         Atom(buf.toString())
     }
     symb
   }

    import Syntax.SExp
    import Syntax.Bool
    val trueVal = Bool(true)
    val falseVal = Bool(false)

   def exprs[T](): List[SExp[T]] = nextSymb() match {
     case EOL | EOF => Nil
     case Ket => nextSymb(); Nil
     case Bra => Syntax.SExps(exprs) :: exprs[T]()
     case Atom(text) => Syntax.Atom[T](text) :: exprs[T]()
     case Num(value) => Syntax.Num[T](value) :: exprs[T]()
     case Str(text) => Syntax.Str[T](text) :: exprs[T]()
     case True => trueVal :: exprs[T]()
     case False => falseVal :: exprs[T]()
   }
  
}
