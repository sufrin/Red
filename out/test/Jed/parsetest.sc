
import RedScript.Test._


rp(""" "\n" """)
rp("     able\nbaker\nfoonly\n")
rp("  able baker \nfoonly")
rp(">>>---<<<\n --==")
rp("xyzzy\n --==")
rp("\n>>>---<<<\n --==")
rp("\n(list >>>---<<< 1 2 3)\n(list --== 1 2 3))")
rep("\n>>>---<<<\n --==")
rep("\n(list >>>---<<< 1 2 3)\n(list --== 1 2 3))")

val u = "\\u"
rp(
  s"""
    "A valid unicode u1234 at the end of a line ${u}1234"
    "A valid unicode uaa0a within a line ${u}aa0a is best"
    "A valid unicode u2283 within a line ${u}2283 is best"
    "A non-unicode (uabc) ${u}abc xyz"
    "A non-unicode (uaXcd) ${u}aXcd xyz"
    "A non-unicode (uabc) at the end of the string (too short) ${u}abc"
    """)

rp(s"""
                   "A non-unicode (uabc) at the end of a line (too short) ${u}abc
                     def"
                """)

rp("""
                   "A string ending at the end of a line (too short)
                     def"
                """)

rp("""
                   "A string ending at the end of a line with a \\\
                     def"
                """)


rp {
  """
    |true
    |false
    |( if true false true)
    |   # comments
    |   # more comments
    |   (' farly) # a line-ending comment
    | position
    |   -->\Oxymoron<-- # three atoms
    |   234
    |   "foobaz"
    |   456
    |""".stripMargin
}

rp {
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
}



