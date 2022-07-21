# Red

Red is no-frills `UNICODE`-capable **modeless** text editor with a simple
implementation that can be customized using the **scala** language.
It has no pretensions to being an all-ecompassing workplace, and
unlike many IDEs does not confuse its user by trying to be helpful: 
everything it does it does in response to user input from
mouse, pad, or keyboard.

Its `undo`/`redo` facility is conventional, but in addition to
this it has  a **cut ring** that retains the last 80 or so **recently
deleted** blocks of contiguous text. This feature is
orthogonal to `undo`/`redo` and makes it straightforward 
to re-insert material that was deleted some time ago. 

--------------

I have used it mainly for preparing **`latex`** and **`markdown`**
manuscripts, as well as programs that don't warrant the burden of
getting to grips with an IDE.

Its support for **`latex`** manuscripts includes the
straightforward manipulation of nested blocks delimited by 
`\begin{...}` and `\end{...}`. 

Its support for **`xml`** includes the
straightforward manipulation of nested blocks delimited by 
`<id ` *parameters*`>` and `</id>`, as well as 
`<id ` *parameters*`/>` constructs.

The **`Tex`** button on the menu bar
generates `pdf` from the (default tex) source manuscript -- usually,
but not necessarily, the manuscript being edited -- using the
script `redpdf`. That script usually invokes a `pdf` viewer.

At the time of writing (July 2022) `redpdf` assumes that the manuscript
is in latex format, and invokes `pdflatex` with appropriate parameters. 


The **`Pandoc`** button on the menu bar
typesets the manuscript being edited -- using the
script `redpandoc`. 

At the time of writing (July 2022) `redpandoc` assumes that the manuscript
is in `markdown+tex_math_dollars` format. 


$$e^{i\pi}+1=0$$

See what I mean?


## Red on Linux

### Installation

The script `~/bin/red` can be used from a terminal to start an edit
session with a running `redserver` (and to start a `redserver` if
one is not already running).

## Red on OS/X

### Installation

### The script `red`
The script `~/bin/red` can be used from a terminal to start an edit
session with a running `redserver` (and to start a `redserver` if
one is not already running).

### The application `Red`
The application **Red** can be installed by copying it to `/Applications`
or to `/Users/`*yourname*`/Applications`.

When started it invokes the script `~/bin/redserver` to run edit
sessions for each file dropped onto it.


### `Tex` and `Skim`
On  `OS/X` the standard `redpdf` script invoked by the `Tex` button
uses `synctex` features to collaborate with the viewer `Skim`.

   1. The current editing position in the manuscript being edited is
   shown as `Skim` opens (or reopens) the newly-generated `pdf` file after the `Tex` 
   button is pressed. 

   2. The current position in the editing session can be changed from `Skim` by
   a `Shift-Cmd-click` on one of the pdf lines.

### History of OS/X Red

These days Apple makes it *almost impossible* to 