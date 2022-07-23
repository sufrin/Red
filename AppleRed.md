# Red

## Introduction

Red is no-frills `UNICODE`-capable **modeless** (and **colourless**)
text editor with a simple implementation that can be customized
using the **scala** language.  It has no pretensions to being an
all-ecompassing workplace, and unlike many IDE and modern editors
does not confuse its user by spontaneously trying to be helpful:
everything it does it does in response to user input from mouse,
pad, or keyboard.

Its `undo`/`redo` facility is conventional, but in addition to
this it has  a **cut ring** that retains the last 80 or so **recently
deleted** blocks of contiguous text. This feature is
orthogonal to `undo`/`redo` and makes it straightforward 
to re-insert material that has -- perhaps inadvertently -- 
been deleted from any editing session running in the same
editor server. The cut ring interface is similar to an
editing session interface, so material can be transferred
from it by copy/paste to any application (including Red)
that has an interface to the system clipboard.

--------------

I have used Red mainly for preparing **`latex`** and **`markdown`**
manuscripts, as well as for preparing programs that don't warrant
the burden of getting to grips with an IDE.

Its support for **`latex`** manuscripts includes the
straightforward manipulation of nested blocks delimited by 
`\begin{...}` and `\end{...}`. 

Its support for **`xml`** includes the
straightforward manipulation of nested blocks delimited by 
`<id ` *parameters*`>` and `</id>`, as well as 
`<id ` *parameters*`/>` constructs.



### `Tex`, `Pandoc`, and `Skim`

The **`Tex`** button on the menu bar generates `pdf` from the
(default tex) source manuscript -- usually, but not necessarily,
the manuscript being edited -- using a script `redpdf` that is
expected to be somewhere on the user's `PATH`.  The `OS/X` `redpdf`
I use assumes that the manuscript is in latex format, and invokes
`pdflatex` with appropriate parameters before invoking the `pdf`
viewer `Skim`.

The **`Pandoc`** button on the menu bar
typesets the manuscript being edited -- using the
script `redpandoc`. The `OS/X` `redpandoc` I use
assumes that the manuscript is in `markdown+tex_math_dollars` format. 
$$e^{i\pi}+1=0$$
See what I mean?

### `SyncTex` features

On  `OS/X` the `redpdf` script invoked by the `Tex` button
uses `synctex` features to collaborate with the viewer `Skim`.

   1. The current editing position in the manuscript being edited
   is shown as `Skim` opens (or reopens) the newly-generated `pdf`
   file after the `Tex` button is pressed.

   2. The current position in the editing session can be changed
   from `Skim` by a `Shift-Cmd-click` on one of the pdf lines.

There are similarly-functioning scripts available for `Linux` that I will
eventually make available. 

### Installation
The `AppleRed.app` directory can be installed either on `OS/X` or on
`Linux` by copying it to `~/Applications`. Before use it is essential
to copy the three Profiles files to your `$(HOME)` and adjust them
to your own requirements.

The profiles are nearly identical -- the important things to change
are the PATHs exported from `.AppleRed-application.profile`; the
other profiles are (but needn't be) essentially the same. The
following command will do the copy.

    cp -pr $(find ~/Applications/AppleRed.app -name ".AppleRed*.profile") ~

The following commands will run from anywhere:

   `$(find ~/Applications/AppleRed.app -name red)` *paths*
   
   `$(find ~/Applications/AppleRed.app -name cred)` *paths*

The former starts an editing server (if it hasn't already started)
to starting editing sessions for the documents denoted in the filestore
by *paths*. The server runs until the last of its editing sessions 
is closed; but keeps running in this case if it was started with
an empty *paths*.

The latter starts its editing sessions independently of any server. 
It terminates when its last editing session is closed.

The `red`, and `cred` scripts function on both operating systems, independently 
of whether the `AppleRed` app has been used. On `OS/X` the app acts as a 
drop-target, or as an open-with subject. When invoked, it starts a
editing server (if necessary), then passes its arguments to it, then
(finally) terminates. Think of it as a "start button" for the server
and don't expect it to hang around between "starts". Drop the `AppleRed.app`
icon onto the `OS/X` dock if you want to make it easily available.


## Editing Sessions

In each editing session window you will see the main text editing
component at the bottom, with three minitext-editing components in a
row above it. The minitexts are labelled **(A)** (*Argument*), **(F)** (*Find*)
 and **(R)** (*Replace*).

The *Argument* minitext provides arguments for many of the other editing
commands, including those on the *File* menu which start new editing
sessions, allow the document to be saved in a different file, and so
forth.

The *Find* and *Replace* minitexts provide arguments for the actions
of the same name. The checkbox next to *Find* indicates whether
its content will be interpreted as a regular expression or as a literal
pattern. The same checkbox  indicates whether the *Replace* content 
will be treated as a substitution for a matching regular expression
or as a literal pattern. 

Many of the editing commands described below are bound to keystrokes, and can
also be used on the minitexts.

Red shows which of its (mini-)text windows has the keyboard focus by
showing its cursor in red. A text window with a greyed-out cursor
doesn't have the keyboard focus. Focus usually moves into a text window
as the mouse-cursor moves into it, but occasionally after popup warnings
it's necessary to force the issue by clicking in the desired window.

## The Editing Model

At any stage, the document in a window (minitext or main) may have
a *selection* -- which is a contiguous region of the document. The
selection is bounded at one end by the *document cursor* -- which
is shown as a thin red rectangle -- and at the other end by the
*mark* -- which is shown as a (thinner) blue rectangle. When the
cursor and mark are in the same position only the cursor is shown,
and the selection (in that document) is effectively empty.

The document cursor can be moved in small increments by the arrow
keys, or can be placed anywhere in the document by clicking with
the left mouse-button. The mark can be placed anywhere in the
document by clicking with the right mouse button.

The text of the selection is always shown on a grey background.

After every editing action the position of the document in the
editor window is adjusted automatically so that *the document
cursor is visible.* Between editing commands the position of the
document in the window can be adjusted manually by the scrollwheel,
or by "dragging" -- moving the mouse with left button pressed.
This can make the document cursor "disappear" temporarily.

There is a *cut-ring* which holds the last several texts copied
or deleted from the document. The very last such text is also
placed on the operating system's clipboard and the most recent
material placed in the system clipboard by Red or any other
application is accessible is *paste*d into the document or *swap*ped
with the current selection when the *Paste* (*Swap*) commands are
invoked (usually by keystroke).

## Editing Commands
The editing actions described below are a small selection of the commands
provided by the editor. 

### Undo and Redo

As editing commands happen their effects are pushed onto the *done stack*.

*Undo* (`C-z`) undoes the topmost effect on the *done stack* and
pushes it onto the *undone stack*.

*Redo* (`C-Z`) re-does the topmost effect on the *undone stack*,
-- pops it from that stack.

When editing commands other than *Undo* or *Redo* happen, the entire
*redo stack* is emptied.

### Cut and Paste

*Cut* (`C-x`, `F1`) removes the selection from the document puts
it in the cut-buffer, and adds it to the cut-ring. 

*Paste* (`C-v`, `F2`) inserts the content of
the cut-buffer into the document and re-selects it. 

*Copy* (`C-c`, `F3`) replaces the cut-buffer with the selection (if there is a
selection).

*SwapSel* (`C-b`, `F4`) exchanges the selection and
the cut-buffer (except that if the selection is null, then the
cut-buffer doesn't change). The replaced text is added to the cut ring
as if it had been deleted.

*SwapCursorAndMark* (`F12`) does what its name suggests.

### Find and Replace in Principle  

In what follows:

  * *following* means "at a position to the right of or below the *editing cursor*."
  * *preceeding* means "at a position to the left of or above the *editing cursor*."

  * **(F)** means "the pattern specified by the *Find* minitext."
  * **(R)** means "the replacement specified by the *Replace* minitext."
  
If the checkbox adjacent to the *Find* minitext is checked then
the find text is interpreted as a (Java-style) regular expression.
Otherwise it is interpreted literally.

When the find text is interpreted as a regular expression, the
replacement text is interpreted as the text obtained (using standard
Java regular-expression substitution rules) by substituting each
instance of `$n` in it by the text that matches the `n`th bracketed
expression in the instance of the pattern that is being replaced.
Otherwise it is interpreted literally.

  * *FindDown* (`C-f`, `Numpad-0`)  selects the nearest *following* instance of **(F)** 

  * *FindUp* `(C-F)`, `Shift-Numpad-0`) selects the nearest *preceeding* instance of **(F)** 


If the current selection is an instance of **(F)** then:

  * *ReplaceDown* `(C-r)', `Numpad-.` saves the current selection in the cut-buffer
  and replaces it with the **(R)** selecting the replacement text
  and leaving the cursor to the right of the mark.

  * *ReplaceUp* `(C-R), `Shift-Numpad-.`` saves the current selection
  in the cut-buffer and replaces it with the **(R)** selecting the
  replacement text and leaving the cursor to the left of the mark.

  * *FindSelDown* (C-A-f) makes the current selection the *Find*
  pattern, turns off regular expression interpretation, and then acts as
  *FindDown*. 

  * *FindSelUp* (C-A-F) makes the current selection the *Find* pattern, 
  then acts as *FindUp*.
  
  * *ClearFind* (C-Numpad-0) clears the find minitext, and 
  moves the editing cursor to that text. 
  
  * *ClearRepl* (C-Numpad-.) clears the replace minitext, and 
  moves the editing cursor to that text. 

### Find and Replace in Practice

One way of replacing the next instance of `"FOO"` with `"BAR"` is to
type the keystrokes bound to

   *ClearFind* `FOO` *FindSelDown* *ClearRepl* `BAR` *ReplaceDown*

Replacing the following instance just requires

   *FindSelDown* *ReplaceDown*

since the *Find* and *Repl* minitexts are already set to the right
pattern and replacement.

  * *ReplaceAll* (on the edit menu) replaces (without any interaction)
    all instances of the *Find* minitext in the current selection
    with the *Repl* minitext, and selects the resulting transformed
    text.  The original selection is preserved in the cut buffer
    (and therefore in the cut ring), so this action can be undone
    immediately with *SwapSel*. 
    
    If you want to "approve" each replacement interactively, then just use
    *FindSelDown* *ReplaceDown* in sequence repeatedly, undoing any
    replacement you don't approve of with *SwapSel*.

### Treatment of the Selection

Red offers two modes of treating the selection. Most users will decide
on one of them and stick to it forever. (I use the second, having grown
accustomed to it in my homegrown editors for more than thirty years.)

1.  **Typing-cuts-selection:** Typing new material automatically cuts
    the selection into the cut buffer (and adds it to the cut ring). 
    This is the behaviour that most people have come to expect of editors.
    
2.  **Typing-removes-mark:** Typing new characters removes the mark, and
    thus deselects the *selection*, but does not delete the selected
    material. In this mode Dred does not distinguish between tentative
    and definite selections.

The choice between treatments is made using the
**File/Preferences/Typing Removes Selection** checkbox. As with all
other preferences its value is preserved between Dred invocations.


##### Key Assignment Policy:

It makes sense for *FindUp* to be bound to the SHIFTED key that
*FindDown* is bound to; and for *ReplaceUp* to be bound to the
SHIFTED *ReplaceDown* key.


## APPENDIX

### History of `AppleRed`

**Red** started life as a program to use as an exemplar for teaching first-year Oxford
undergraduates a short course in object-orient programming using
**`scala`**. It was to be the basis of a number of practical exercises.

Its editing model was identical to that used in the **dred** editor I had built
from scratch in **`java`** in the early 2000's and had been using ever since. 
**Dred** had  developed from a sequence of editors, all using the same
*modeless* editing model, derived from an abstract formal specification
I had published in the late 1970's.

It wasn't long before I decided that it would be a good idea to
make **Red** more than a toy; and soon after I started its development
(during the first pandemic lockdown period) it became my editor of
choice. Shortly afterwards I found it straightforward to make it
integrable with `OS/X` as an app, as well as runnable  as an ordinary
`JVM`-compatible program on `OS/X`, `Linux`, and `Windows`.

My intention was to remove some of its more advanced features for
use in the course, and this is what I did: the result was a simpler
editor, **jed**, published for my students as the course started
in Trinity term in the spring of 2022.

The `undo`/`redo` feature of **jed** had not been implemented in
**Red**, because the latter's cut-ring feature met all my reasonable
needs; so as soon as the course was over (early summer 2022) I
started to enhance **jed** with the features I had cut out of the
earlier **red**. The result was what I now call **AppleRed**.

The last spasm of **AppleRed**'s development was occasioned by a
change in the `OS/X` security model for apps. To paraphrase my
obituary for the unitary AppleRed app that provided editing sessions
through its embedded server as well as providing the more usual
drag-drop, and open-with functionality.  server that it contains:
   
   ----------------------------------------------------------
   "On `Catalina` (versions above 10.14) and its successors the
   situation is complicated by (welcome, but ferocious) Apple
   security measures that *forbid* the reception of messages (of
   any kind) by uncertified apps. At the time of writing I see no
   reasonable way round these measures (and, believe me, I have
   looked) that can be taken without paying the **Apple Ransom**,
   which imposes both a financial and an intellectual burden. I
   have therefore decided to distribute the program as a simple
   `OS/X app` that invokes a helper program to provide editing
   session functionality."
   ----------------------------------------------------------
