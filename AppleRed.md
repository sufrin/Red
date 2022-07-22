# Red

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
