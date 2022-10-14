package RedScript

class SourcePosition(val pos: (String,Int,Int)) extends AnyVal {
  def line: Int = pos._2
  def col:  Int = pos._3
  def path: String = pos._1
  override def toString: String =
     if (line<0) "" else s"$path@$line:$col"
}
