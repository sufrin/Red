package Red

object SafeConfiguration {
  def apply(): String =
"""
#
# SAFE Red profile and bindings in RedScript (derived from ~/.red/profile.redscript 4th Feb 2023, but without includes)
#

##############################################################################
#
# Persistent features for the profile
#
constant font:pref "Dejavu Sans Mono"
constant font:fams (list font:pref "Monospaced")

PROFILE:select  font:style   "Features" "Font Style"  "plain" (list "plain" "bold")
PROFILE:select  font:size    "Features" "Font Size"   18 (list 12 14 16 18 20 24 28)
PROFILE:select  font:family  "Features" "Font Family" font:pref font:fams

PROFILE:bool    mathkeyboard "Features" "Mathematical Keyboard"  true
PROFILE:bool    develop      "Features" "Development"            false
PROFILE:bool    monitoring   "Features" "Monitoring"             false
#
#############################################################################

# syntactic sugar for several global constant declarations
(val (monitor ↦ (if monitoring UI:popup (fun x ())))
     (user    ↦ (ENV "USER"))
     (os      ↦ (PROP "os.name"))
     (OSX     ↦ (<=  "Mac" os))
)

(monitor (SOURCE)
         user
         os
         (if OSX "OSX" "Linux")
         (string "Cut Ring: " (UI:cutringBound))
)

#############################################################################
#
#
#       Declare fonts and their roles
#
constant font:A (UI:font (string font:family "/" font:style "/" font:size))
constant font:B (UI:font (string font:family "/" font:style "/" (- font:size 2)))
constant font:C (UI:font (string "Dialog" "/" "bold" "/" (max font:size 16)))

monitor (SOURCE) (list font:family font:style font:size) font:A font:B font:C

UI:useFont font:A widget default button menu menubutton feedback
UI:useFont font:B menu menubutton feedback
UI:useFont font:C menu menubutton button
#
#
#
#############################################################################


#############################################################################
#
#
#       Declare features of the UI
#
constant shell:Commands (list "wc" "ls -lt" "date" "printenv")

(def (CONFIG:pipeShellCommands path) shell:Commands)

#
#       Latex menu is to be present for .tex files / what's on the menu
#
(def (CONFIG:needsLatex      path)
     (if (endsWith path ".tex") (seq (LOADTEX:) true) false))

(def (CONFIG:latexBlockTypes path) latex:blocktypes)


(constant latex:blocktypes
  `(      foil     itemize   enumerate        -
          note     exercise  answer           -
          code     "-code"   "code*"  alltt   -
          center   verbatim  comment  smaller -
          question part      ans
  )
)

# Latex snippets are placed on the \begin{...}/Tex menu
#
#
(def (CONFIG:latex:Snippets path) (LATEX:Snippets.toList))

constant LATEX:Snippets (queue:new)

# Add a snippet declaration with LATEX:snippet "name" “long string”
(defForm (LATEX:snippet env tag text)
         (LATEX:Snippets.enq (tag ↦ text))
         ())

# bind a keystroke to a styling action
(def (STYLE: key before after)
     #(log key before after)
     (UI:keys ( key ↦ (UI:styleAs before after) )))

(def (LOADTEX:)
     # Main keyboard
     (STYLE: "SLASH(C)"            “\textit{” “}”)
     (STYLE: "SLASH(SC)"           “\textsl{” “}”)
     (STYLE: "BACKSLASH(SC)"       “\emph{”   “}”)
     (STYLE: "BACKSLASH(C)"        “\textbf{” “}”)
     (STYLE: "4(C)"                “$”        “$”)
     (STYLE: "BACKQUOTE(C)"        “\texttt{” “}”)
     # Numeric keyboard
     (STYLE: "Numpad*@Numpad"      “\textbf{” “}”)
     (STYLE: "Numpad*(S)@Numpad"   “\emph{”   “}”)
     (STYLE: "Numpad/@Numpad"      “\textit{” “}”)
     (STYLE: "Numpad/(S)@Numpad"   “\textsl{” “}”)

     (UI:abbrev  "\\f"                "\\footnote{}")
     (UI:abbrev  "\\s"                "\\section{}")
     (UI:abbrev  "\\ss"               "\\subsection{}")
     (UI:abbrev  "\\sss"              "\\subsubsection{}")
)

#
#
#
#############################################################################

#############################################################################
#
#    Declaration notation for specification of alt-keystrokes
#
#       alt: ch ins             -- alt-ch       inserts ins
#       ALT: ch ins'            -- alt-shift-ch inserts ins'
#       ALTS: ch ins ins'       == both the above
#
(def (alt: ch ins)
     (UI:keys ( (string "'" ch "'(A)")  ↦ (insert (string ins)) )))

(def (ALT: ch ins)
     (UI:keys ( (string "'" ch "'(AS)") ↦ (insert (string ins)) )))

(def (ALTS: ch insUnshifted insShifted)
  (seq
     #(log "ALTS: " ch insUnshifted insShifted)
     (UI:keys
         ( (string "'" ch "'(A)")  ↦ (insert (string insUnshifted)) )
         ( (string "'" ch "'(AS)") ↦ (insert (string insShifted))   )
     )))
#
#
#
#############################################################################

#############################################################################
#
# indents, etc on the keypad
#
(constant ArrowKeys
          (UI:newEventMap
             ("LEFT(SC)"  ↦ (UI:commandNamed "removeCommonPrefix"))
             ("LEFT(C)"   ↦ (UI:commandNamed "undentSelection"))
             ("RIGHT(C)"  ↦ (UI:commandNamed "indentSelection"))
             ))
#
#
#
#############################################################################


#############################################################################
#
#
#       Report unhandled input
#
PROFILE:bool    quietignore  "Features" "Silence Undefined Keys" false

#
(def (CONFIG:unhandledInput key where)
     (if quietignore
         ()
         (UI:popup (string:cat "Undefined Keystroke from " where) " " (UI:inputToString key))))

(def (CONFIG:eventMap path)
     (if* ((endsWith path ".tex") ↦ texEvents)
          ((endsWith path ".md")  ↦ mdEvents)
          ( true ↦ UI:defaultEventMap)))

(constant texEvents
          (UI:newEventMap UI:defaultEventMap ArrowKeys
          ))

(constant mdEvents
          (UI:newEventMap UI:defaultEventMap ArrowKeys
          ))
#
#
#############################################################################

#############################################################################
#
#
#       Experimental scripts for the foot of the "Pipe" menu
#

# evaluate the selection
def  (Eval path arg find repl sel) (readEval sel false)

constant PIPE:transformNames (queue:new `(Eval))

###############################
#
#  Pipe-transforms are systematic rewrites of the selection using (collections of) pattern=>template
#  substitutions. They are presently delivered at the foot of the Pipe menu
#
###############################

# (PIPE:transform translations) constructs a pipe-transform from translations, where
#    translations is a list of find-replace specs of the form (pat=>template) or (pat template)
#    the replacements are applied simultaneously, with matching priority from the left
#    replacements are independently specified, and
#    each pattern's groups are numbered individually in its corresponding template
#    (with $0 denoting the entire match)
#
(def (PIPE:transform translations)
     (val (patterns  => (re:new false (translations.map (fun (p) (p.fst)))))
          (templates => (translations.map (fun (p) (p.snd))))
          (fun (path arg find repl sel) (patterns.replace sel templates))))

# (PIPE:transform translations...) constructs a pipe-transform from translations..., where
#    translations... is any number of specs of the form (pat=>template) or (quote pat template)
(defForm (PIPE:transform* (env => translations))
         (PIPE:transform (translations.map (fun (t) (eval env t)))))



#
# (defTransform name translations) declares a named pipe-transform from translations, where
#    translations is any number of specs of the form (pat => template) or (quote pat template) or `(pat template)
#
(defForm (defTransform (env => args))
         (val (name         => (hd args))
              (translations => (tl args))
              (seq (monitor (string "defTransform " name " " translations))
                   (val
                     (theFun => (PIPE:transform (translations.map (fun (t) (eval env t)))))
                     (seq
                          (eval (list `constant name theFun))
                          (PIPE:transformNames.enq name))
                   )
              )
         )
)

#   Examples: (all definitions yield the same transform)
##  defTransform ABBARROW  ("B" ↦ "A")  (‘\\usepackage({[^}]+})’ ↦ ‘useful$1’)
##  defTransform ABBA     `("B" "A")   `(‘\\usepackage({[^}]+})’ ‘useful$1’)
##  defTransform ABBQ      (quote "B" "A") (quote ‘\\usepackage({[^}]+})’ ‘useful$1’)

(def (CONFIG:pipeRedScripts path) (PIPE:transformNames.toList))
(def (CONFIG:needsPandoc    path) (endsWith path ".md"))

#
#
#
#############################################################################

"""

}
