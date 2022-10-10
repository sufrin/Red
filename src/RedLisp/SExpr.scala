package RedLisp

trait Env[T] {
  def apply(name: String): SExp[T]
  /**
   * The environment that extends this by binding
   * successive variables in the pattern to successive
   * `values`. This will later permit pattern matching.
   */
  def extend(pattern: SExp[T], values: List[SExp[T]]): Env[T]
}

trait SExp[T] {
  def eval(env: Env[T]): SExp[T]
  def isNull:   Boolean = false
}

case class Var[T](name: String)           extends SExp[T] {
  def eval(env: Env[T]): SExp[T] = env(name)
}

case class Seq[T](elements: List[SExp[T]])  extends SExp[T] {
  override def isNull = elements.isEmpty
  def eval(env: Env[T]): SExp[T] =
    if (elements.isEmpty) this else  {
       val operator = elements.head.eval(env)
       operator match {
         case Subr( scala ) =>
              val args = Seq(elements.tail.map(_.eval(env)))
              scala(env, args)

         case FSubr( scala ) =>
           val args = Seq(elements.tail)
           scala(env, args)

         case Expr(env, params, body) =>
           val args = elements.tail.map(_.eval(env))
           body.eval(env.extend(params, args))

         case other =>
           Error(s"$operator is not applicable", Some(this))
       }
  }

}


/**
 * The fixedpoints of `eval` are represented as `Const`s.
 */
trait Const[T] extends SExp[T] {
  def eval(env: Env[T]): SExp[T] = this
}

case class Num[T](value: Int) extends Const[T]

case class Bool[T](value: Boolean) extends Const[T]

case class Expr[T](env: Env[T], pattern: SExp[T], body: SExp[T])
     extends Const[T]

case class Subr[T](scala: (Env[T], SExp[T]) => SExp[T])
     extends Const[T]

case class FSubr[T](scala: (Env[T], SExp[T]) => SExp[T])
     extends Const[T]

case class Prim[T](value: T) extends Const[T]

case class Error[T](why: String, description: Option[SExp[T]])  extends scala.Error with Const[T]

class PairList[T](pairs: List[(String, SExp[T])], derivedFrom: Option[PairList[T]] = None) extends Env[T] {
  def apply(name: String): SExp[T] =
    {
      for { (n, v) <- pairs if (n==name) } return v
      if (derivedFrom.isEmpty) Error(s"Unbound variable $name", None) else derivedFrom.get(name)
    }

  def extend(pattern: SExp[T], values: List[SExp[T]]): Env[T] = {
    pattern match {

      case Var(name) =>
        val newPairs: List[(String, SExp[T])] = List[(String, SExp[T])](name, Seq[T](values))
        new PairList[T](newPairs, Some(this))

      case Seq(patterns) if patterns.length==values.length && patterns.forall(_.isInstanceOf[Var[T]]) =>
        val newPairs = patterns.map { case Var(v) => v }.zip(values)
        new PairList[T](newPairs, Some(this))

        /** One layer of matching  */
      case _ => throw Error(s"Pattern match error $values", pattern)

    }

  }

  override def toString: String =
      if (derivedFrom.isEmpty)
        pairs.mkString("(", "->", ")")
      else
        s"${pairs.mkString("(", "->", ")")}\n${derivedFrom.get}"
}

object Primitives {
  val primitives = new PairList[Unit] ( List(
    // TODO: do something with rest
    "lambda" -> FSubr { case (env, Seq(params :: body :: rest)) => Expr(env, params, body) },
    // TODO: do something with rest
    "if"     -> FSubr { case (env, Seq(pred :: thenPart :: elsePart :: rest)) =>
                        pred.eval(env) match {
                          case Bool(true) => thenPart.eval(env)
                          case Bool(false) => elsePart.eval(env)
                          case other => throw Error(s"condition evaluates to a non-Bool: $other", Some(pred))
                        }
                      }
  ))
}