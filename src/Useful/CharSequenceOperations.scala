package Useful

import scala.collection.Iterator

object CharSequenceOperations {

  implicit class WithCharSequenceOps(val chars: CharSequence) extends AnyVal {

    def forwardIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = 0
      def hasNext: Boolean = i < chars.length
      def next(): Char =
          if (hasNext) { val c = chars.charAt(i); i += 1; c } else '\u0000'
    }

    def reversedIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = chars.length
      def hasNext: Boolean = i > 0
      def next(): Char = if (hasNext)  { i -= 1; chars.charAt(i) } else '\u0000'
    }

    def reversedIterator(upTo: Int): Iterator[Char] = new Iterator[Char] {
      var i: Int = upTo
      def hasNext: Boolean = i > 0
      def next(): Char = if (hasNext) { i -= 1; chars.charAt(i) } else '\u0000'
    }

    /**
     *  Map a well-formed hexit sequence to Some(`Long`)
     *  else yield `None`
     */
    def hexToLong: Option[Long] = {
      var n: Long    = 0
      var wellFormed = true
      for { c <- chars.forwardIterator() }
        if ('a'<=c&&c<='f')  n = n*16+c-'a'+10 else
        if ('A'<=c&&c<='F')  n = n*16+c-'A'+10 else
        if ('0'<=c&&c<='9')  n = n*16+c-'0' else wellFormed = false
      if (wellFormed) Some(n.toChar) else None
    }

    /**
     *  Map a well-formed unicoded escape sequence `\uxxxx` sequence to `Some(Char)`
     *  else yield `None`
     */
    def toUnicode: Option[Char] = {
      var n: Long = 0
      val it         = chars.forwardIterator()
      val slosh      = it.next()
      val u          = it.next()
      var wellFormed = slosh=='\\' && (u=='u' || u=='U')
      if (wellFormed) for { c <- it }
        if ('a'<=c&&c<='f')  n = n*16+c-'a'+10 else
        if ('A'<=c&&c<='F')  n = n*16+c-'A'+10 else
        if ('0'<=c&&c<='9')  n = n*16+c-'0' else wellFormed = false
      if (wellFormed) Some(n.toChar) else None
    }

  }

}
