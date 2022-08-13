package Useful

import scala.collection.Iterator

object CharSequenceOperations {

  implicit class WithCharSequenceOps(val chars: CharSequence) extends AnyVal {

    def forwardIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = 0
      def hasNext: Boolean = i < chars.length
      def next(): Char = { val c = chars.charAt(i); i += 1; c }
    }

    def reversedIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = chars.length
      def hasNext: Boolean = i > 0
      def next(): Char = { i -= 1; chars.charAt(i) }
    }

    def reversedIterator(upTo: Int): Iterator[Char] = new Iterator[Char] {
      var i: Int = upTo
      def hasNext: Boolean = i > 0
      def next(): Char = { i -= 1; chars.charAt(i) }
    }

  }

}
