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

  case object Eof extends Symbol

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

   def getNext(): Char = if (source.hasNext) in.next() else { in.ch = '\u0000'; in.ch }

   def hasNextSymb: Boolean = in.ch != '\u0000'

   def nextSymb(): Lexical.Symbol = in.ch match {
     case '\u0000' => Eof
     case ')' => getNext(); Ket
     case '(' => getNext(); Bra
     case '#' =>
       while (getNext()!='\n') {}
       nextSymb()
     case '\n' =>
       while (getNext()=='\n') {}
       nextSymb()
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

  
}
