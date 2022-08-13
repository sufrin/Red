package Useful

import scala.collection._
import CharSequenceOperations._

/**
 *   Represents a mutable finite mapping, {{{map: Seq[Char] -> [T]}}}
 *
 *   The representation is a tree, whose edges are individual letters
 *   of the character sequences of the domain, and whose nodes contain
 *   range elements when appropriate.
 *
 *   For example:
 *   {{{
 *     "F"->1, "FOO"->2, "FOOLS" -> 3,"FOX" -> 4,
 *     "BOG" -> 5, "BOOT" -> 6
 *   }}}
 *   is represented as the tree (shown on its side)
 *   {{{
 *  F -> 1
 *    O
 *      O -> 2
 *        L
 *          S -> 3
 *      X -> 4
 *  B
 *    O
 *      G -> 5
 *      O
 *        T -> 6
 *   }}}
 *
 *
 *   To find the datum, `t`, if any, associated with the sequence `s[0..n)`,
 *   follow the path s(0), s(1), ... from the root node: this will reach
 *   a node 'm', which has value either `None`, or `Some(t)`.
 *
 *   The methods of the trait `mutable.Map` are implemented by
 *   inheritance from the scala library mixin of the same name.
 *
 *   The immutable map valued variable `suffixes` is used to
 *   map each edge to its subtree. We could have used a mutable
 *   map here, but the stock immutable map has less overhead, being
 *   in essence a list of pairs. The ordering of the arcs in the
 *   representation of each node is effectively indeterminate unless
 *   some constraint is placed on the ordering of its pairs.
 */

class   PrefixMap[T]
  extends mutable.Map[CharSequence, T]
{
  private var suffixes: immutable.Map[Char, PrefixMap[T]] = immutable.Map.empty
  private var value:    Option[T]                         = None

  def get(s: CharSequence): Option[T] = get(s, 0)

  /**  Pre:    0<=from<=s.length
       Return: if (suffix in dom map) then Some(map(suffix)) else None
               where suffix = s drop from
   */
  private def get(s: CharSequence, from: Int): Option[T] =
    if (from==s.length)
      value
    else
      suffixes get (s.charAt(from)) flatMap (_.get(s, from+1))

  /**
   * Yield the value of the mapping at longest prefix of `s` that
   * matches one of the mapping's domain elements, together with the length
   * of that prefix.
   */
  def follow(s: CharSequence): Option[(T, Int)] = follow(s.forwardIterator())

  def followBackwardsFrom(upTo: Int, s: CharSequence) = follow(s.reversedIterator(upTo))

  /**
   * Yield the value of the mapping at longest prefix of `it` that
   * matches one of the mapping's domain elements, together with the length
   * of that prefix. If there is no such prefix, yield `None`.
   *
   */
  def follow(it: Iterator[Char]): Option[(T, Int)] =
  { // Inv:
    //   result==Some(t, n) =>
    //     n <= edges      &&
    //     map(it take n toString)==t &&
    //     there is no i st. n<i<edges && map(it take i toString)==t
    //
    var result: Option[(T, Int)] =  None

    // if non-null, the node reached from the root by the path so far traversed
    // namely `it take edges`.
    var node:   PrefixMap[T]   = this

    // the number of edges so far traversed
    var edges                   = 0
    while (it.hasNext && (node ne null)) {
      if (node.value.nonEmpty) result = Some(node.value.get, edges)
      node = node.suffixes.getOrElse(it.next(), null)
      edges += 1
    }
    if ((node ne null) && node.value.nonEmpty) result = Some(node.value.get, edges)
    result
  }


  /**
   *  Returns the (shared) subtree of `map` at the path `s drop it.length`.
   *  The tree is extended, if necessary, by adding additional nodes
   *  for the characters along the given path.
   */
  private def prefixed(it: Iterator[Char]): PrefixMap[T] =
    if (!it.hasNext)
      this
    else
    { val first = it.next()
      suffixes get first match {
        case None =>
          suffixes = suffixes + (first -> empty)
        case _ =>
      }
      suffixes(first) prefixed (it)
    }

  /**
   *  Returns the (shared) subtree of `map` at the path `s`.
   *  The tree is extended, if necessary, by adding additional nodes
   *  for the characters along the given path.
   */
  def prefixed(s: CharSequence): PrefixMap[T] =
    prefixed(s.forwardIterator())

  /**
   *   Post: map=map0 + s -> v
   */
  override def update(s: CharSequence, t: T) =
    prefixed(s).value = Some(t)

  /**
   *   Post: map=map0 + s.reverse -> v
   */
  def reverseUpdate(s: CharSequence, t: T) =
    prefixed(s.reversedIterator()).value = Some(t)

  /**
   *   Post:   map = map0 \ {s}
   *   Return: if s in dom(map0) then Some(map0(s)) else None
   */
  override def remove(s: CharSequence): Option[T] = remove(s, 0)

  private def remove(s: CharSequence, from: Int): Option[T] =
    if (from==s.length)
    { val prev = value
      value = None
      prev
    }
    else suffixes get (s.charAt(from)) flatMap (_.remove(s, from+1))

  /**
   *  Return an iterator that yields the pairs of the maplet.
   */
  def iterator: Iterator[(String, T)] =
    (for {v  <- value.iterator } yield ("", v)) ++ // the value, if any
      (for {(chr, map) <- suffixes   // the suffix mappings
            (s,   v)   <- map        // results from the suffix mappings
            }
      yield (s"$chr$s", v))

  /** An iterator that returns the pairs of the mapping, with
   *  domain elements represented as lists of characters.
   */
  def pathIterator: Iterator[(List[Char], T)] =
    (for {v  <- value.iterator } yield (Nil, v)) ++ // the value, if any
      (for {(chr, map) <- suffixes        // the suffix mappings
            (s,   v)   <- map.pathIterator  // results from the suffix mappings
            }
      yield (chr :: s, v))

  def paths: Iterator[List[Char]] =
    for { (path, _) <- pathIterator }  yield path


  /**
   *  Augment the mapping with `pair`,  and return the mapping itself.
   */
  def addOne(pair: (CharSequence, T)): this.type =
  { update(pair._1, pair._2); this }

  /**
   *  Diminish the mapping by removing `s` from its domain, and return the mapping itself.
   */
  def subtractOne(s: CharSequence): this.type = { remove(s); this }

  /** A string representing the
   * structure of the representation
   * of the mapping
   */
  def show: String = repString(1)

  private def repString(indent: Int): String = {
    val b = new StringBuilder
    if (value.nonEmpty) {
      b append " -> "
      b append (value.get.toString)
    }
    b append ('\n')
    for { (ch, tree) <- suffixes}
    { b append ("  "*indent)
      b append (s"$ch")
      b append (tree.repString(indent+1))
    }
    b.toString
  }

  @inline override def empty = new PrefixMap[T]
}

object PrefixMap {

  /**
   * Construct a new mapping from pairs provided in situ
   */
  def apply[T](pairs:  (CharSequence, T)*): PrefixMap[T] = apply(pairs)

  /**
   *  Construct a new mapping from an iterable generator of pairs
   */
  def apply[T](pairs: Iterable[(CharSequence, T)]): PrefixMap[T] =
  { val map = new PrefixMap[T]
    for { pair <- pairs } map += pair
    map
  }

  /**
   *  Construct a new mapping from an iterator of pairs
   */
  def apply[T](pairs: Iterator[(CharSequence, T)]): PrefixMap[T] =
  { val map = new PrefixMap[T]
    for { pair <- pairs } map += pair
    map
  }


}
