import RedScript.Test.SourceCode

"""
  |("foobar" cat "baz" 23 " " 35)
  |""".stripMargin.rep

"""
  |(constant re (re:regex "([A-Z]*)([a-z]*)([0-9]*)"))
  |constant m1 (re match "Advocate23")
  |(m1 group 1)
  |(m1 groups)
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
   (val (a.1)(b.2)(+ a b))
""".rep

