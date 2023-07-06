import RedScript.Test.SourceCode
"""
  | (`a=(+ 3 4))
  | `a.7
  | (& 3 2 (- 0x1))
  | (- 0x7007)
  | ((list 1 2 3).map(fun (x) (+ x x)))
  | (list:map (list "1" "2" "3") (fun (x) (x.cat x)))
  | (list:map (list "1" "2" "3") (fun x (x.cat x)))
  | ((list 1 2 3 4 5 6 7 8).filter(fun (x) (> x 5)))
  """.stripMargin.rep

"""
  |("foobar".cat "baz" 23 " " 35)
  |(constant a (quote a b c))
  |(a.cat (quote 3 4))
  |(`(quote one two).cat (quote 3 4))
  |""".stripMargin.rep

"""
  |(constant re (re:regex "([A-Z]*)([a-z]*)([0-9]*)"))
  |constant m1 (re.match "Advocate23")
  |(m1.group 1)
  |(m1.groups)
  |""".stripMargin.rep

"""
  |constant abc (re:regex "((c+)(b+)(a+))")
  |variable subj "the ccccccbbbbbbbaaa"
  |variable match (re:find abc subj)
  |match
  |(re:groups match)
  |""".stripMargin.rep


"""
   (list "wc" "ls -lt" "date" "printenv")
   (constant shellCommands (list "wc" "ls -lt" "date" "printenv"))
   constant shambles (list "wc" "ls -lt" "date" "printenv")
   constant scrambles [list "wc" "ls -lt" "date" "printenv"]
   (list shellCommands
   shambles
   scrambles
   )
   (def (pipeShellCommands path) shellCommands)
   [def (pipeShellCommandsx path) shellCommands]
   pipeShellCommands
   pipeShellCommands "phooey"
   (val (a=1)(b=2)(+ a b))
""".rep

