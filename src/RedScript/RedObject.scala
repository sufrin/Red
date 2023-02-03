package RedScript

import scala.annotation.nowarn

object RedObject {
  import Language._

  /** Experimental object-centred calls  */
  object RegexMethods {
    val lookup: collection.immutable.HashMap[String, SExp] = collection.immutable.HashMap[String, SExp](
      "match" -> Subr("re:match", {
        case List(REGEX(regex), Str(text)) =>
          regex.matches(text, 0, text.length) match {
            case None => nil
            case Some(theMatch) => REGMATCH(theMatch)
          }
        case _ => throw RuntimeError(s"re:match: REGEX -> nil | REGMATCH")
      }),
      "replace" -> Subr("re:replace", {
        case REGEX(regex) :: Str(text) :: SExpSeq(templates) :: rest =>
          val literal = rest match {
            case Nil => false
            case List(Bool(b)) => b
          }
          val (count, changed) = regex.rewriteAll(text, templates.map { case Str(str) => str}, literal)
          Str(if (count==0) text else changed)
      }),
      "find" -> Subr("re:find", {
        case List(REGEX(regex), Str(text)) =>
          regex.findPrefix(text, 0, text.length) match {
            case None => nil
            case Some(theMatch) => REGMATCH(theMatch)
          }
        case List(REGEX(regex), Str(text), Num(from)) =>
          regex.findPrefix(text, from.toInt, text.length) match {
            case None => nil
            case Some(theMatch) => REGMATCH(theMatch)
          }
        case List(REGEX(regex), Str(text), Num(from), Num(to)) =>
          regex.findPrefix(text, from.toInt, to.toInt, text.length) match {
            case None => nil
            case Some(theMatch) => REGMATCH(theMatch)
          }
        case _ => throw RuntimeError(s"re:find: REGEX STRING Num? Num? -> nil | REGMATCH")
      })
    )

    def apply(name: String): SExp = lookup.getOrElse(name, Nothing)
  }

  object RegMatchMethods {
    val lookup: collection.immutable.HashMap[String, SExp] = collection.immutable.HashMap[String, SExp](
      "span" -> Subr("re:span", {
        case List(REGMATCH(theMatch)) => SExpSeq(List(Num(theMatch.start), Num(theMatch.end)))
        case _ => throw RuntimeError(s"re:span: REGMATCH->[Num,Num]")
      }),
      "subst" -> Subr("re:subst", {
        case List(REGMATCH(theMatch), Str(theTemplate)) => Str(theMatch.substitute(theTemplate))
        case _ => throw RuntimeError(s"re:subst: REGMATCH STRING -> STRING")
      }),
      "group" -> Subr("re:group", {
        case List(REGMATCH(theMatch), Num(i)) => Str(theMatch.group(i.toInt))
        case _ => throw RuntimeError(s"re:group: REGMATCH Num -> STRING")
      }),
      "groups" -> Subr("re:groups", {
        case List(REGMATCH(theMatch)) => SExpSeq(theMatch.groups.map(Str).toList)
        case _ => throw RuntimeError(s"re:groups: REGMATCH -> [STRING]")
      })
    )

    def apply(name: String): SExp = lookup.getOrElse(name, Nothing)
  }

  case class REGEX(regex: sufrin.regex.Regex) extends Obj {
    override def toString: String = s"(re:regex \"${regex.toString()}\")"
    def method(name: String): SExp = RegexMethods(name)
  }

  case class REGMATCH(regmatch: sufrin.regex.Regex.StringMatch) extends Obj {
    override def toString: String = s"$regmatch"
    def method(name: String): SExp = RegMatchMethods(name)
  }

  /** String methods */
  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object StrMethods {
    val cat = Subr("string:cat", {
      case values =>
        val res = new StringBuilder
        for { v <- values } res.append(v.toPlainString)
        Str(res.toString())
    })

    val range = Subr("string:range", {
      case List(Str(text), Num(from), Num(to)) => Str(text.subSequence(from.toInt, to.toInt).toString)
    })

    def apply(name: String): SExp = name match {
      case "cat"   => cat
      case "range" => range
      case _       => Nothing
    }
  }

  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object PairMethods {
    val fst = Subr("pair:fst", {
      case List(Pair(l, r)) => l
    })
    val snd = Subr("pair:snd", {
      case List(Pair(l, r)) => r
    })
    def apply(name: String): SExp = name match {
      case "fst" => fst
      case "snd" => snd
      case _ => Nothing
    }
  }

  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object MapMethods {
    def add(args: List[SExp]): SExp = {
      val map :: maps = args
      var EnvExpr(res) = map
      for {m <- maps} m match {
        case EnvExpr(env) => res = res + env
        case Pair(Str(d), r) => res = new LocalEnv(List((d, r)), Some(res))
        case Pair(d, r) => res = new LocalEnv(List((d.toPlainString, r)), Some(res))
      }
      EnvExpr(res)
    }

    def apply(name: String): SExp = name match {
      case "add" => Subr("map:add", add)
      case _ => Nothing
    }
  }

  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object TableMethods {

    case class TABLE(map: scala.collection.mutable.Map[SExp,SExp]) extends Obj {
        /** Returns a metjhod body: for the moment a subr */
        override def method(name: String): SExp = apply(name)
      }

    def add(args: List[SExp]): SExp = {
      val TABLE(map) :: SExpSeq(maplets) :: Nil = args
      for {m <- maplets} m match {
        case Pair(d, r) =>
          map.addOne((d, r))
        case SExpSeq(List(d, r)) =>
          map.addOne((d, r))
      }
      Nothing
    }

    def keys(args: List[SExp]): SExp = {
      val TABLE(map) :: Nil = args
      SExpSeq(map.keys.toList)
    }

    def eval(args: List[SExp]): SExp = {
      val TABLE(map) :: key :: Nil= args
      map.getOrElse(key, Nothing)
    }

    def apply(name: String): SExp = name match {
      case "add"  => Subr("table:add", add)
      case "keys" => Subr("table:keys", keys)
      case "eval" => Subr("table:eval", eval)
      case "new" => Subr("table:new", {
        case Nil => TABLE(new collection.mutable.LinkedHashMap[SExp,SExp])
      })
      case _     => Nothing
    }
  }


  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object QueueMethods {
    case class QUEUE(queue: collection.mutable.Queue[SExp]) extends Obj {
      /** Returns a metjhod body: for the moment a subr */
      override def method(name: String): SExp = apply(name)
      override def toString: String = queue.mkString("(queue:new(quote ", " ", "))")
    }

    def apply(name: String): SExp = name match {
      case "new" => Subr("queue:new", {
        case Nil => QUEUE(new collection.mutable.Queue[SExp])
        case List(SExpSeq(sexps)) =>
             val res = new collection.mutable.Queue[SExp]
             res.enqueueAll(sexps)
             QUEUE(res)
      })
      case "enq" => Subr("queue:enq", {
        case (List(QUEUE(queue), sexp)) => queue.enqueue(sexp); Nothing
      })
      case "deq" => Subr("queue:deq", {
        case (List(QUEUE(queue))) => queue.dequeue()
      })
      case "isEmpty" => Subr("queue:isEmpty", {
        case (List(QUEUE(queue))) => Bool(queue.isEmpty)
      })
      case "clear" => Subr("queue:clear", {
        case (List(QUEUE(queue))) => queue.clear(); Nothing
      })
      case "size" => Subr("queue:size", {
        case (List(QUEUE(queue))) => Num(queue.size)
      })
      case "toList" => Subr("queue:toList", {
        case (List(QUEUE(queue))) => SExpSeq(queue.toList)
        case (List(QUEUE(queue), Bool(clear))) =>
           val res = SExpSeq(queue.toList)
           if (clear) queue.clear()
           res
      })
      case _ => Nothing
    }
  }
  /** Sequence methods */
  @nowarn("msg=not.*?exhaustive") // nonexhaustive matches are deliberate
  object SexpSeqMethods {
    val lookup: collection.immutable.HashMap[String, SExp] = collection.immutable.HashMap[String, SExp](
      "range" -> Subr("list:range", {
        case List(SExpSeq(elts), Num(from), Num(to)) => SExpSeq(elts.drop(from.toInt).take((to - from).toInt))
      }),
      "nth" -> Subr("list:nth", {
        case List(SExpSeq(elts), Num(n)) => elts(n.toInt)
      }),
      "cat" -> Subr("list:cat", {
        case List(SExpSeq(k0), SExpSeq(k1)) => SExpSeq(k0++k1); case other => throw RuntimeError(s"malformed ++: ${SExpSeq(other)}")
      }),

    "map" -> Subr("list:map", {
      case List(SExpSeq(elts), Expr(env1, params, body)) =>
        SExpSeq(elts.map { case arg: SExp => body.eval(env1.extend(params, List(arg))) })
      case List(SExpSeq(elts), Subr(name, scala)) =>
        SExpSeq(elts.map { case arg: SExp => scala(List(arg)) })
      case List(SExpSeq(elts), ExprAll(env1, params, body)) =>
        SExpSeq(elts.map { case arg: SExp => body.eval(env1.extend(params, List(arg))) })
      case other => throw RuntimeError(s"malformed list:map: $other")
    }),
    "fst" -> Subr("list:fst", {
        case List(SExpSeq(e1::elts)) => e1
        case other => throw RuntimeError(s"malformed list:fst: $other")
      }),
    "snd" -> Subr("list:snd", {
        case List(SExpSeq(_ :: e2 :: elts)) => e2
        case other => throw RuntimeError(s"malformed list:snd: $other")
      }),
    "filter" -> Subr("list:filter", {
        case List(SExpSeq(elts), Expr(env1, params, body)) =>
          SExpSeq(elts.filter { case arg: SExp => body.eval(env1.extend(params, List(arg))).truth })
        case List(SExpSeq(elts), ExprAll(env1, params, body)) =>
          SExpSeq(elts.filter { case arg: SExp => body.eval(env1.extend(params, List(arg))).truth })
        case other => throw RuntimeError(s"malformed list:map: $other")
      })
    )
    def apply(name: String): SExp = lookup.getOrElse(name, Nothing)
  }

}
