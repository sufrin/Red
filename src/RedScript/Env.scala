package RedScript

import RedScript.Language._

import scala.annotation.tailrec

trait Env {
  thisEnv =>

  def apply(name: String): Option[SExp]

  def print(level: Int=0): Unit

  def maplets: List[(String, SExp)]

  def +(that: Env): Env = new LocalEnv(that.maplets, Some(this))

  /**
   * The environment that extends this by binding
   * successive variables in the pattern to successive
   * `values`.
   * TODO: proper pattern matching for use in a match expression
   */
  def extend(pattern: SExp, values: List[SExp]): Env = {
    val newEnv =
    pattern match {
      case Variable(name) =>
        val newPairs: List[(String, SExp)] = List((name, Language.SExpSeq(values)))
        new LocalEnv(newPairs, Some(this))

      case SExpSeq(patterns) if patterns.forall(_.isInstanceOf[Variable]) =>
        if (patterns.length==values.length)
           new LocalEnv(patterns.map {  case Variable(v) => v }.zip(values), Some(thisEnv))
        else
           throw RuntimeError(s"${patterns.length} variables ${values.length} values: binding $patterns to $values")

      case SExpSeq(patterns) if !patterns.forall(_.isInstanceOf[Variable]) =>
        throw SyntaxError(s"Pattern must be a sequence of variables: binding $patterns to $values")

      /** One layer of matching */
      case other =>
        throw SyntaxError(s"Pattern must be a variable or sequence of variables: binding $pattern to $values")
    }

    //newEnv.print()
    newEnv
  }
}

class LocalEnv(pairs: List[(String, SExp)], derivedFrom: Option[Env] = None) extends Env {

  def apply(name: String): Option[SExp] =
  { LocalEnv.find(name, pairs) match {
      case None => if (derivedFrom.isEmpty) None else derivedFrom.get(name)
      case some => some
    }
  }

  def maplets: List[(String, SExp)] = pairs ++ (if (derivedFrom.isEmpty) Nil else derivedFrom.get.maplets)

  def print(level: Int): Unit = {
    Console.print(" "*level)
    Console.println("«")
    for  { (name, value) <- pairs } {
      Console.print(" "*(1+level))
      Console.println(s"$name -> $value")
    }
    if (derivedFrom.nonEmpty) derivedFrom.get.print(level+1)
    Console.print(" "*(level)); Console.println("»")
  }

  object LocalEnv {
    @tailrec def find(name: String, pairs: List[(String, SExp)]): Option[SExp] =
      pairs match {
        case Nil => None
        case (n, v)::pairs if n==name => Some(v)
        case _::pairs                 => find(name, pairs)
      }
  }

  override def toString: String =
    if (derivedFrom.isEmpty)
      pairs.map{case (n, v) => s"($n->$v)"}.mkString("(", " ", ")")
    else
      s"${ pairs.map{case (n, v) => s"($n->$v)"}}\n${derivedFrom.get}"
}

class MutableEnv extends LocalEnv(Nil, None) {
  import scala.collection.mutable

  val map = new mutable.LinkedHashMap[String, SExp]

  override def maplets: List[(String, SExp)] = map.iterator.toList

  override def apply(name: String): Option[SExp] = map.get(name)

  override def toString: String = map.mkString("(", " ", ")")

  override def print(level: Int): Unit = if (level==0) {
    Console.print(" "*level)
    Console.println("««")
    for  { (name, value) <- map } { Console.print(" "*(1+level)); Console.println(s"$name -> $value") }
    Console.print(" "*level)
    Console.println("»»")
  }

  def define(name: String, value: SExp): Unit =
    map.get(name) match {
      case None    => map.put(name, value)
      case Some(v) => throw new RuntimeError(s"Redefinition of global $name")
    }

  def set(name: String, value: SExp): Unit = map.put(name, value)

  def clear(): Unit = map.clear()

}
