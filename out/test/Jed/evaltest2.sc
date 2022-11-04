import RedScript.Test.SourceCode
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

