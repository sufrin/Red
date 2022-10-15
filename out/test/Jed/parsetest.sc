import RedScript._

val s1 =
  """
    |true
    |false
    |( if true false true)
    |   # comments
    |   # more comments
    |   (' farly) # a line-ending comment
    |   -->\Oxymoron<-- # three atoms
    |   234
    |   "foobaz"
    |   456
    |""".stripMargin

val s2 =
  """
    |23
    |24
    |(if true 23 24)
    |(def fst(x y) x)
    |(def snd(x y) y)
    |(fst 3 4)
    |(snd 3 4)
    |list 1 2 3 (fst 4 5) (list 6 "seven" 7 8)
    |-->> Oxymoron <<-
    |(((foo ))]
    |""".stripMargin


//val p1 = new Parser(Source.fromString(s1))
//while (p1.hasNextSymb) { print(s"$p1: "); println(p1.nextSymb()) }


//Test.rp(s1)

Test.rp(s2)
Test.rp("able baker foonly\n")
Test.rp("able baker foonly")





