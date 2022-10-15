


val s1  =
  """  zsugar
    |
    |
    |
    |("foo" x)
    |(foo x)
    |
    |def f(a b c) b
    |? "A line"
    |' (c d e)
    |"A bracketed line"
    |set a (fun (x) (cons x (list 'c 'd 'e)))
    |a
    |'Z
    |(a 'Z)
    |f (list 'c 'd 'e) 'hooh 'hah
    |eval '(a 'Y)
    |set ay (list 'a ''Y)
    |ay
    |eval ay
    |cons 'k ay
    |(null ay)
    |(hd ay)
    |(tl ay)
    |(null nil)
    |(3 4 5)
    |(def copy(xs) (if* ((null xs) (hd xs))
    |                  ( true
    |                   (cons (hd xs)
    |                         (copy (tl xs))
    |                   )))))
    |(copy ay)
    |
    |(def cat(xs ys)
    |     (if (null xs)
    |         nil
    |         (cons (hd xs)
    |               (cat (tl xs) ys)
    |         )
    |      )
    |)
    |
    |(cat ay ay)
    |(cat (dog ay) ay)
    |37
    |(def f(3)4)
    |(def pr(title xs) (seq (print title) (print xs) xs))
    |(pr "foo" ay)
    |
    |(def prr(title xs) (print title) (print xs) xs)
    |(prr "foo" ay)
    |
    |""".stripMargin


RedScript.Test.rep(s1)

val s2=
  """
     (+ 1)
     (* 1 2 3)
     variable m1 (- 0 1)
     m1
     set m1 (+ m1 m1)
     m1
     def m1()  ()
     set m1 (* 3 m1)
     m1
     * m1 3 4 "foo"
     / 12 2 3
     - 12 2 3
     -3
     --><--
  """
RedScript.Test.rep(s2)

val u = "\\u"
RedScript.Test.rep(
  s"""
    "${u}1234"
    "${u}aa0a is best"
    "Mistake ${u}abc xyz"
    "Mistake ${u}aXc xyz"
    "Underflow ${u}abc"
    "Underflow ${u}abc
    def"
    "Unclosed string
   """)

