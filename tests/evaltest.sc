import RedScript.Test._

"""(1 . "foo") """.rep
"""list (1 . "foo") (2 . "bar") `( 3 . "foobaz" )""".rep
"""list (1 . "foo") (2 . "bar") `( 3 . ("foobaz" is best))""".rep
"""list (1 . "foo") (2 . "bar") `( 3 . ("foobaz" is best)""".rep
"""list (1 . "foo") (2 . "bar") `( 3 . "foobaz" is best)""".rep
"""+ 3 4 """.rep
" :: 1 (list 2 3 4) ".rep
"constant aaa 3\naaa".rep
"""+ 3 "x" """.rep
"""`(`ENV "USER")""".rep
"""`(ENV "USER")""".rep
"""(ENV "USER")""".rep
"""(PROP "os.name")""".rep
"`(a b c)".rep
"""(string "foo" "bar" 23 "pig")""".rep
"""<= "Mac" (PROP "os.name")""".rep
"(seq 1 2 3)".rep
"(+)".rep
"(null)\n(null 42)\n(:: `a 3)".rep
"(def (Ω a b c) 43 (+ a b c))".rep
"(def (ΩΩ a b c) (seq 43 (+ a b c)))".rep
"""(def (ΩΩΩ . all) (println "all: " all))""".rep
"Ω\nΩΩ".rep
"(Ω 1 2 3)\n".rep
"(ΩΩΩ 1 2 3)\nΩΩΩ".rep
"-->\nisSymb -->\nisAtom -->\nisVar -->\nisVar `abc".rep
"= + 3\n= (+ 1 2) 3\n".rep
"= (list 1 2) (list (- 2 1) (+ 1 2))".rep
"= (list 1 3) (list (- 2 1) (+ 1 2))".rep
"nil".rep
"< 2 2\n< 2 3\n<= true false\n<= false true".rep
"""(val (a . 1) (b . 2) (+ a b))
  (var (a . 1) (b . 2) (seq (:= a 3)
                        (:= a (+ a b))
                        (println "a: " a)))
  (val a 3 b 4 3)
  (val a 3 (b 4) b)
  """.rep

""" "USING FEXPRS"
  |(defForm (fe (env . args))
  |         (seq (println args)
  |              (println (eval env (hd args)))
  |              (println (tl args))))
  |fe
  |
  |(val (a . "This is a")
  |     (fe a b (c d e)))
  |
  |(defForm (IF env cond tp fp)
  |         (if* ((eval env cond) . (eval env tp))
  |              (true            . (eval env fp))))
  |
  |IF
  |
  |(defForm (IFF env cond tp fp)
  |         (if* ((eval env cond) . (eval env tp))
  |              (eval env fp)))
  |              
  |(IF (= 3 3) (println `three) (println `nonthree))
  |(IFF (= 3 4) (println `three) (println `nonthree))
  |  |""".stripMargin.rep

"""
  def (until a b) (if (<= b a) () (:: a (until (+ 1 a) b))))
  def (to a b) (if (< b a) () (:: a (to (+ 1 a) b))))
  (until 0 10)
  (to 0 10)
  (eval (:: `* (to 1 5)))
  (eval (:: `+ (to 1 5)))
""".eval


  """def (>> x) (println x)
    | >> "Three runtime errors"
    | zsugar
    |("foo" x)
    |(foo x)
    | >> "A definition"
    |def (f a b c) b
    |f
    |>> "A sequence of print; yields the value of the last of them"
    |? "the" "rain" "in" "spain" (SOURCE)
    |seq (println) (println "the" "rain" "in" "spain")
    |seq (println) (println (until 0 10))
    |>>"A quoted list"
    |`(c d e)
    |>>"Evalulation of a quoted list"
    |eval `(println `d `e)
    |>>"An unbracketed composite sexpr on a single line"
    |constant a (fun (x) (:: x (list `c `d `e)))
    |a
    |>>"This is a quotation over three lines"
    |`(this is
    |  a quotation
    |  over three lines)
    | >> "A strange identifier"
    | (constant åçé 35)
    | åçé
    | >> "Symbols can be bound, too."
    | def (∫ a b c)(list `∫ a b c)
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
    |:: `k ay
    |(null ay)
    |(hd ay)
    |(tl ay)
    |(null nil)
    |
    |(def (copy xs) (if* ((null xs) . nil)
    |                    ( true .
    |                      (:: (hd xs)
    |                          (copy (tl xs))))
    |                   )))
    |(copy ay)
    |
    |(def (cat xs ys)
    |     (if (null xs)
    |         ys
    |         (:: (hd xs)
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
    |(def (prr (title xs)) (print title) (print xs) xs)
    |(prr "foo" ay)
    |
    |""".stripMargin.rep

  """def (>>> x) (println "Expecting: " x)
     >>> 1
     (+ 1)
     >>> 6
     (* 1 2 3)
     >>> true
     (&& (= 3 3) (= 4 4))
     >>> true
     (|| (= 3 4) (= 4 4))
     >>> "type error"
     (&& (= 4 4) 5 (= 3 4))
     >>> "m1 to be declared as -1"
     variable m1 (- 0 1)
     >>> "-1"
     m1
     >>> "m1 to be doubled by assignment"
     := m1 (+ m1 m1)
     >>> "-2"
     m1
     >>> "report of a redefinition of m1"
     def (m1)  ()
     >>> "tripling m1 by assignment"
     := m1 (* 3 m1)
     >>> "-6"
     m1
     >>> "type error report"
     * m1 3 4 "foo"
     >>> "2, then 7"
     / 12 2 3
     - 12 2 3
     >>> "a quoted symbol"
     --><--
  """.eval


