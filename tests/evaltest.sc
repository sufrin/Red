
RedScript.Test.rep("(def f(a b c) (+ a b c))\n(f 1 2 3)\n")
RedScript.Test.rep("= + 3\n= (+ 1 2) 3\n")
RedScript.Test.rep("= (list 1 2) (list (- 2 1) (+ 1 2))")
RedScript.Test.rep("= (list 1 3) (list (- 2 1) (+ 1 2))")

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
    |constant a (fun (x) (cons x (list 'c 'd 'e)))
    |a
    |'Z
    |(a 'Z)
    |f (list 'c 'd 'e) 'hooh 'hah
    |eval '(a 'Y)
    |variable ay (list 'a ''Y)
    |ay
    |eval ay
    |cons 'k ay
    |(null ay)
    |(hd ay)
    |(tl ay)
    |(null nil)
    |(3 4 5)
    |(def copy(xs) (if' ((null xs) (hd xs))
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
     (printenv)
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

