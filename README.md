

## Introduction


`Red` (also known as `AppleRed`) is an efficient, no-frills
*unicode-capable* mode-free (and colourless) text editor with a
simple implementation and user interface that can be customized
using the `redscript` language (a Lisp-like notation).  Its underlying
capabilities can extended straightforwardly using `scala`.  It has
no pretensions to being an all-encompassing workplace, and unlike
many IDEs and modern editors does not confuse its user by *spontaneously*
trying to be helpful: everything it does it does in response to
user input from mouse, pad, or keyboard.

## Unicode


`Red` comes equipped with configuration facilities that make working with a
wide range of unicode characters straightforward.  Configurations may
include mnemonic abbreviations for character sequences that include unicode
symbols. For example (material to the right of # is a comment):

~~~~~
    UI:abbrev  "exists"    "\u2203"  # ∃
    UI:abbrev  "notexists" "\u2204"  # ∄
    UI:abbrev  "~∃"        "\u2204"  # ∄
    UI:abbrev  "in"        "\u2208"  # ∈
    UI:abbrev  "notin"     "\u2190"  # ∉
    UI:abbrev  "<-"        "\u2190"  # ←
 
    UI:abbrev "lb"         "pound"
    UI:abbrev "lbs"        "pounds and pounds"
~~~~~

When the abbrev key (often `ESC`) is pressed at the right of a
mnemonic in text, that mnemonic is replaced by the corresponding
character (sequence).

They may also bind specific keystrokes to actions that insert
character sequences that include unicode symbols. For example,
this (perhaps-eccentric) declaration binds `ALT`, and `ALT-SHIFT`
of the up, down, left, and right keys to various unicode arrows.

~~~~~
    (UI:keys
      ("LEFT(SA)"   ↦ (insert "⇐"))
      ("RIGHT(SA)"  ↦ (insert "⇒"))
      ("LEFT(A)"    ↦ (insert "←"))
      ("RIGHT(A)"   ↦ (insert "→"))
      ("UP(SA)"     ↦ (insert "⇑"))
      ("DOWN(SA)"   ↦ (insert "⇓"))
      ("UP(A)"      ↦ (insert "↑"))
      ("DOWN(A)"    ↦ (insert "↓"))
    )
~~~~~


The following declarations bind `ALT` variants of some ordinary keys
to unicode characters.

~~~~~
    (ALTS: "," "←" "≤") # map ALT "," to "←" and ALT-SHIFT "," to "≤"
    (ALTS: "." "→" "≥")
    (ALTS: "1" "⇓" "⇑")
    (ALTS: "6" "↓" "↑")
    (ALTS: "7" "∧" "∨")
    (ALTS: "o" "∘" "•")
    (ALT:  "A" "∀")
    (ALT:  "E" "∃")
    (ALT:  "Z" "↯")
~~~~~



## Tex


`Red` was designed to work comfortably with Latex/Xelatex manuscripts,
and to that end implements single-click actions that make the flow
of work in producing, typesetting, and proofreading such manuscripts
straightforward.
It also comes equipped with actions to support convenient selection and
motion of nested `\begin`-`end` blocks.

## Markdown


Markdown manuscripts are also straighforward to work with; though
the varieties of markdown-like manuscript languages and dialects
means that some local configuration ***may be necessary*** in order to
work with a particular dialect.



        Bernard Sufrin
        Oxford, May and December 2022