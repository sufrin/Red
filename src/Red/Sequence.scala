package Red

import scala.reflect.ClassTag

/** An extensible editable sequence of elements
 *
 *  '''Represents'''
 *  {{{
 *         elements: Seq[T]
 *  }}}
 *
 *  '''Technicality'''
 *
 *  The `ClassTag` constraint on the type parameter `T`  is present so that
 *  concrete arrays of `[T]` can be built. It won't require any special
 *  action from clients of this class.
 *
 *  Copyright (C) B.A. Sufrin, 2021 and J.M. Spivey. 2015
 *
 *  The present implementation is one component of the refactored and redesigned
 *  document representation (`Text`) used in Mike Spivey's original "ewoks" program.
 *
 *  In the documentation of `Sequence` and its subclasses we use the infix operators `↑, ↓`
 *  to denote the `take` and `drop` functions over abstract sequences, and the expression
 *  `e†n` to denote a sequence of `n` copies of `e`.
 *
 *  In other words:  `s↑n = s[0..n)`, and `s↓n = s[n..#s)`.
 *
 *  @see `Red.CharacterSequence`
 *  @see `Red.Document`
 */

class Sequence [T: ClassTag](initialSize: Int=20) extends scala.collection.mutable.Seq[T] {
  /**
   * '''Represents''' {{{
   *  elements: Seq[T]
   * }}}
   *
   * '''Abstraction invariant'''
   * {{{
   *  elements = buffer↑l ++ buffer↓r
   * }}}
   * '''Invariant'''
   * {{{
   *         0<=l<=r<=buffer.length
   *         length = l+buffer.length-r
   * }}}
   *
   * '''Pictorially'''
   * {{{
   *  buffer: [xxxxxxxxxxxxx) GAP [xxxxxxxxxxxxx) buffer.length
   *                         l     r
   * }}}
   */
  protected
  var buffer: Array[T] = new Array[T](initialSize)
  protected
  var l: Int      = 0
  protected
  var r: Int      = buffer.length

  /**  The name of the class (for assertion failure reports)*/
  override protected def className: String = this.getClass.getName

  /**
   * '''Invariant'''
   * {{{
   *   length = #elements
   * }}}
   */
  var length = 0

  /** '''Returns''' {{{ buffer.length - length }}} which is the remaining unoccupied space in the buffer */
  @inline protected def gapSize: Int = buffer.length-length

  /** Construct a sequence from the given `initial` elements */
  def this(initial: Iterable[T]) = {
    this()
    insert(0, initial)
  }

  import Sequence._

  /**
   * '''Post''' {{{ elements=[] }}}
   */
  def clear(): Unit = {
    l        = 0
    length   = 0
    r        = buffer.length
  }

  /**
   * Ensure that the gap is no smaller than `size`, expanding the buffer if necessary.
   * As an heuristic, the buffer size is doubled if this will suffice.
   * This is on the grounds that expanding requires an O(#elements) copy
   * and should be undertaken as infrequently as possible.
   */
  protected
  def ensureFits(size: Int): Unit =
    if (size > gapSize) {
      if (logging)
        finest(s"$className.ensureFits[gapSize=$gapSize]($size)")
      val newLength = 2*buffer.length max length+size
      val newBuf    = new Array[T](newLength)
      val rLength   = buffer.length - r
      System.arraycopy(buffer,  0, newBuf, 0, l)
      System.arraycopy(buffer,  r, newBuf,  newLength - rLength, rLength)
      buffer = newBuf
      r = l + gapSize
    }

  /**
   *  Move the gap so that it starts at `position`
   *
   *  '''Pre''' {{{ 0<=position<=position+gapSize<=buffer.length }}}
   *
   *  ''' Post''' {{{ elements==elements0 && l==position && r==l+gapSize }}}
   *
   */
  protected
  def moveGapTo(position: Int): Unit = {
    // while ( l < position ) { buffer(l)=buffer(r); l+=1; r+=1 }
    // while ( l > position ) { l-=1; r-=1; buffer(r)=buffer(l) }
    if (logging)
       finest(s"$className.moveGap($position) [0..$l)" +
             s" [$r..${buffer.length})")
    if (l < position)
      // System.arraycopy(buffer, r, buffer, l, position - l)
      copyBuffer(from=r, to=l, count=position-l)
    else if (l > position)
      // System.arraycopy(buffer, position, buffer, r - l + position, l - position)
      copyBuffer(from=position, to=r-(l-position), count = l - position)
    l = position
    r = l+gapSize
  }

  @inline private def copyBuffer(from: Int, to: Int, count: Int): Unit = {
    if (logging)
        finest(s"$className.copyBuffer($from, $to, $count)")
    System.arraycopy(buffer, from, buffer, to, count)
  }

  /** Insert `count` copies of `elt` at `position`
   *
   * '''Pre''' {{{ 0 <= position <= #elements }}}
   * '''Post''' {{{ elements=elements0 ↑ position ++ [elt†count] ++ elements0 ↓ position }}}
   *
   * */
  def insert(position: Int, elt: T, count: Int=1): Unit = {
    assert(0 <= position && position<=length,
           s"Sequence(#elements=$length).insert($position, $elt, $count)")
    ensureFits(count)
    moveGapTo(position)
    for { _ <- 0 until count } { buffer(l)=elt; l += 1 }
    length += count
  }

  /** Insert all elements from the (finite) iterable, `elts`,  starting at position.
   *
   * '''Pre''' {{{ 0 <= position <= #elements }}}
   * '''Post''' {{{ elements=elements0 ↑ position ++ elts ++ elements0 ↓ position }}}
   *
   * */
  def insert(position: Int, elts: Iterable[T]): Unit = {
    assert(0 <= position && position<=length,
           s"$className[#elements=$length].insert($position, $elts)")
    var i = position
    for { t <- elts } { insert(i, t); i += 1 }
  }

  /**
   *
   * Delete `count` elements starting at `position`
   *
   * '''Pre''' {{{ 0 <= position <= #elements && position+count<=#elements }}}
   * '''Post''' {{{ elements=elements0 ↑ position ++ elements0 ↓ (position+count) }}}
   *
   */
  def delete(position: Int, count: Int = 1): Unit = {
    assert(position<=length && position+count<=length,
           s"$className[#elements=$length].delete($position,$count)")
    moveGapTo(position)
    r += count
    length -= count
  }

  /**
   *
   * '''Pre''' {{{ 0<=n<#elements }}}
   * '''Returns''' {{{ elements(n) }}}
   */
  def apply(n: Int): T = {
    if (logging) finest(s"$className[#elements=$length].element($n)")
    assert(0<=n && n<length, s"$className[#elements=$length].apply($n)")
    elements(n)
  }
  /**
   * Unchecked inline
   * '''Pre''' {{{ 0<=n<#elements }}}
   * '''Returns''' {{{ elements(n) }}}
   */
  @inline
  protected def elements(n: Int): T = if (n<l) buffer(n) else buffer(n-l+r)

  /** Replace the element at `position` with `elt`
   *
   * '''Pre''' {{{ 0 <= position < #elements }}}
   * '''Post''' {{{ elements=elements0 ↑ position ++ [elt] ++ elements0 ↓ 1 ↓ position }}}
   *
   * */
  def update(n: Int, elt: T): Unit = {
    assert(0<=n && n<length,
           s"$className[#elements=$length].update($n, $elt)")
    if (n<l) buffer(n)=elt else buffer(n-l+r)=elt
  }

  /**
   * '''Post''' {{{ elements = elements0 ++ [elt] }}}
   */
  @inline def append(elt: T): Unit = insert(length, elt)

  /**
   * '''Post''' {{{ elements = [elt] ++ elements0 }}}
   */
  @inline def prepend(elt: T): Unit = insert(0, elt, 1)

  def iterator: Iterator[T] = new scala.collection.Iterator[T] {
    var i=0
    def next(): T = { val t = Sequence.this(i); i+= 1; t }
    def hasNext: Boolean = i<Sequence.this.length
  }

  /** String form of the sequence of elements */
  override def toString(): String = {
    val r = new StringBuilder("⟨")
    r.append(elements(0).toString)
    for { i <- 1 until length } {
      r.append(", ")
      r.append(elements(i).toString)
    }
    r.append("⟩")
    r.toString()
  }

  /** String form of the sequence of elements, avoiding side-effects */
  def asString: String = toString()

  /** String form of the internal representation of the sequence, avoiding side-effects  */
  def repString: String =
      s"${this.asString} ([0..$l), [$r..${buffer.length}), $length})"

}

object Sequence extends Logging.Loggable


