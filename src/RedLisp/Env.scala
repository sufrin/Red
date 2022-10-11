package RedLisp

import RedLisp.Syntax._

trait Env[T] {
  def apply(name: String): Const[T]

  def print(): Unit

  /**
   * The environment that extends this by binding
   * successive variables in the pattern to successive
   * `values`. This will later permit pattern matching.
   */
  def extend(pattern: SExp[T], values: List[Const[T]]): Env[T] = {
    pattern match {

      case Atom(name) =>
        val newPairs: List[(String, Const[T])] = List((name, Syntax.Seq(values)))
        new LocalEnv[T](newPairs, Some(this))

      case SExps(patterns) if patterns.length == values.length && patterns.forall(_.isInstanceOf[Atom[T]]) =>
        val newPairs = patterns.map { case Atom(v) => v }.zip(values)
        new LocalEnv[T](newPairs, Some(this))

      /** One layer of matching */
      case _ => throw Error(s"Pattern match error $pattern\n$values")

    }
  }
}

class LocalEnv[T](pairs: List[(String, Const[T])], derivedFrom: Option[Env[T]] = None) extends Env[T] {
  def apply(name: String): Const[T] =
  {
    for { (n, v) <- pairs if (n==name) } return v
    if (derivedFrom.isEmpty) Error[T](s"Unbound variable $name") else derivedFrom.get(name)
  }

  def print(): Unit = {
    for  { (name, value) <- pairs } println(s"$name -> $value")
    println("==========")
    if (derivedFrom.nonEmpty) derivedFrom.get.print()
  }

  override def toString: String =
    if (derivedFrom.isEmpty)
      pairs.mkString("(", "->", ")")
    else
      s"${pairs.mkString("(", "->", ")")}\n${derivedFrom.get}"
}

class MutableEnv[T] extends LocalEnv[T](Nil, None) {
  import scala.collection.mutable

  val map = new mutable.LinkedHashMap[String, Const[T]]

  override def apply(name: String): Const[T] =
    map.getOrElse(name, Error(s"Unbound variable $name"))

  override def toString: String = map.mkString("(", "->", ")")

  override def print(): Unit = {
    for  { (name, value) <- map } println(s"$name -> $value")
    println("==========")
  }

  def define(name: String, value: Const[T]): Unit = map.put(name, value)

  def set(name: String, value: Const[T]): Unit = map.put(name, value)

}
