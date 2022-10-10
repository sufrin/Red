

/*
val s1 =
  """
    |23
    |24
    |(if true 23 24)
    |(def fst(x y) x)
    |(def snd(x y) y)
    |(fst 3 4)
    |(snd 3 4)
    |list 1 2 3 (fst 4 5) (list 6 7 8)
    |""".stripMargin

Test.rep(s1)
*/

val s2 =
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


RedScript.Test.rep(s2)

