import RedScript.Test._

"(seq 1 2 3)".rep
"(def Ω(a b c) 43 (+ a b c))".rep
"(def ΩΩ(a b c) (seq 43 (+ a b c)))".rep
"Ω\nΩΩ".rep
"(Ω 1 2 3)\n".rep
"= + 3\n= (+ 1 2) 3\n".rep
"= (list 1 2) (list (- 2 1) (+ 1 2))".rep
"= (list 1 3) (list (- 2 1) (+ 1 2))".rep
"nil".rep
"< 2 2\n< 2 3\n<= true false\n<= false true".rep
"""(val (a 1) a)
  |
  |""".stripMargin.rep

"""
  def until (a b) (if (<= b a) () (cons a (until (+ 1 a) b))))
  def to (a b) (if (< b a) () (cons a (to (+ 1 a) b))))
  (until 0 10)
  (to 0 10)
  (eval (cons `* (to 1 5)))
  (eval (cons `+ (to 1 5)))
""".eval


  """def >>(x) (seq)
    | >> "Three runtime errors"
    | zsugar
    |("foo" x)
    |(foo x)
    | >> "A definition"
    |def f(a b c) b
    |f
    |>> "A sequence of print; yields the value of the last of them"
    |? "the" "rain" "in" "spain"
    |seq (println) (println "the" "rain" "in" "spain")
    |seq (println) (print "the" "rain" "in" "spain")
    |>>"A quoted list"
    |` (c d e)
    |>>"An unbracketed composite sexpr on a single line"
    |constant a (fun (x) (cons x (list `c `d `e)))
    |a
    |>>"This is a quotation over three lines"
    |`(this is
    |  a quotation
    |  over three lines)
    | >> "A strange identifier"
    | (constant åçé 35)
    | åçé
    | >> "Symbols can be bound, too."
    | def ∫(a b c)(list `∫ a b c)
    | ∫ 1 2 3
    | constant π `π
    | π
    | (eval π)
    |
    |`Z
    |(a `Z)
    |f (list `c `d `e) `hooh `hah
    |eval `(a `Y)
    |variable ay (list `a ``Y)
    |ay
    |eval ay
    |cons `k ay
    |(null ay)
    |(hd ay)
    |(tl ay)
    |(null nil)
    |
    |(def copy(xs) (if' [(null xs) nil]
    |                   [true
    |                    (cons (hd xs)
    |                          (copy (tl xs)))]
    |                   )))
    |(copy ay)
    |
    |(def cat(xs ys)
    |     (if (null xs)
    |         ys
    |         (cons (hd xs)
    |               (cat (tl xs) ys)
    |         )
    |      )
    |)
    |
    |(cat ay ay)
    |(cat (dog ay) ay)
    | >> "Miscellaneous, including some errors"
    |(def f(3)4)
    |
    |(def prr(title xs) (print title) (print xs) xs)
    |(prr "foo" ay)
    |
    |""".stripMargin.rep

  """
     (+ 1)
     (* 1 2 3)
     (&& (= 3 3) (= 4 4))
     (|| (= 3 4) (= 4 4))
     (&& (= 4 4) 5 (= 3 4))
     variable m1 (- 0 1)
     m1
     := m1 (+ m1 m1)
     m1
     def m1()  ()
     := m1 (* 3 m1)
     m1
     * m1 3 4 "foo"
     / 12 2 3
     - 12 2 3
     -3
     --><--
  """.rep


