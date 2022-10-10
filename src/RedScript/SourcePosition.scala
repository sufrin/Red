package RedScript

class SourcePosition(val pos: (String,Int,Int)) extends AnyVal {
  override def toString: String = s"${pos._1}@${pos._2}:${pos._3}"
}
