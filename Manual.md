------------------------------
title: "Example PDF"
author: [Author]
date: "2022-03-04"
subject: "Markdown"
keywords: [Markdown, Example]
lang: "en"
toc: true
toc-own-page: true
...
------------------------------
Dred
====

(May 2017)

Contents
--------

-   [Unix/Linux Usage](#Unix/Linux%20Usage)
-   [Windows (XP) Usage](#Windows%20(XP)%20Usage)
-   [Mac OS X Usage](#MACOS)
-   [PDFSync](#PDFSync)
-   [The Dred Editing Session
    Window](#The%20Dred%20Editing%20Session%20Window)
-   [Editing Model](#Editing%20Model)
-   [Editing Actions](#Editing%20Actions)
-   [Mouse Actions](#Mouse%20Actions)
-   [Opening and Saving files](#Opening%20and%20Saving%20files)
-   [Fonts](#Fonts)
-   [Preferences](#Preferences)
-   [Etc](#Etc)
-   [Brief History](#Brief%20History)
-   [Back to Dred Help Index](index.html)

Dred is a *lightweight*, customizeable, and extensible Unicode text
editor. It comes, complete with documentation, in a single (350K) jar,
and needs a 1.8 (or later) JRE to run.

[]{#Unix/Linux Usage}

Unix/Linux Usage
----------------

            java -jar Dred.jar [--enc=encoding] [--bindings=bindings-url] --wait files 

or

            java -jar Dred.jar [--enc=encoding] [--bindings=bindings-url] -w files 

This starts edit sessions for each of the files. The program exits when
the last session terminates.

            java -jar Dred.jar [--enc=encoding] [--bindings=bindings-url] files  &

This starts edit sessions for each of the given files, *using the
user\'s existing Dred server if one is running*, then exits immediately.
If no server is running then it starts itself in server mode, puts up an
(iconified) server control, and starts edit sessions for the filenames.
Files are usually specified by paths in the local filestore, but can
also be specifed by URL: see [Opening a URL](#Opening%20a%20URL) for
details.

If `--bindings=...` is specified then the editor\'s bindings are taken
from the file specified by the given URL; otherwise the default bindings
are used. If `--enc=...` is specified, then the files are assumed to use
the specified encoding; otherwise UTF8 is assumed.

Bindings files describe

-   the connections between keystrokes and editor actions
-   abbreviations for Unicode symbols and their corresponding shortcut
    keystrokes
-   loading and configuration of Dred extensions

(see also [Specifying Bindings](Bindings.html) and [Under the
Hood](Underhood.html))

The above commands are often embedded in simple shell scripts.

**Remark (May 2017)**: I have, in recent years, used the following
script (named `dred`) to invoke the editor. Using this script the first
invocation automatically establishes a server.

    #/bin/sh
    #
    # shell command to invoke dred (as a server if necessary)
    #
    nohup java -jar ~/bin/Dred.jar "$@" > /dev/null 2>&1 &

\
[]{#MACOS}

Mac OS X Usage
--------------

Command-line usage is much the same as Linux/Unix usage (as outlined
above), except that it makes sense to drive `dred` with a shell script
containing the following command \-- which enables some of the OS X
specific features of java, and assumes `Dred.jar` is in `$HOME/Bin`.

    exec java    -Xdock:icon="$HOME/Bin/dnought.png" \
                 -Xdock:name="Dred"                  \
                 -Dapple.laf.useScreenMenuBar=true -jar $HOME/Bin/Dred.jar "$@" 

The `AppleDred.app` application is available from the Dred website and
can also be installed into your local `Applications` directory or the
system-wide `/Applications` directory. It is registered as handling all
files. It\'s a good idea to pin it to the taskbar but it isn\'t
necessary. Launching AppleDred on login can be slightly problematic
unless you have ensured that it will pick up the right shell
environment. The most important shell environment variables it uses are
`PATH` and (if you use the Latex Tool) `TEXINPUTS`. Launching it for the
first time (ie starting it in server mode) in a login session from a
terminal window will cause it to pick these up from the terminal\'s
shell envionment. **Remark (May 2017)**: the most recent variants of
(Unix) Dred execute all the shell commands that it uses in the
environment set by `source ~/.dred_profile` (if such a file exists). The
one I use looks like this:

    # Establish the environment for shell-calls from Dred
    # Prefix a path if it isn't already present
    function CONSPATH ()
    { (printenv PATH | grep -q -F ":${1}
    ${1}:") || export PATH=${1}:$PATH
    }

    # Append a path if it isn't already present
    function PATHCONS ()
    { (printenv PATH | grep -q -F ":${1}
    ${1}:") || export PATH=$PATH:${1}
    }

    PATHCONS           /usr/texbin
    CONSPATH           $HOME/bin

    export TEXINPUTS=.:~/Tex//:
    export TEXFONTS=:.:~/Tex//:

The application file `AppleDred.app/Contents/Java/AppleDred.jar` can be
started from the command-line as described elsewhere in the
documentation. If the application is already running, then it will be
used to service the command given on the command-line; if it isn\'t then
a non-Application server will be started and thenceforth used to service
command-line commands and subsequent invocations of the application.
**The important difference is that a server started as the application
is a little more careful when `Apple-Q` is pressed, and will not let you
lose your work without asking whether this is what you mean.**

The following shell script can be used from the command line. It
facilitates \'\'equal opportunity\'\' operation of `dred` from command
line and Finder.


    #!/bin/sh
    # OSX shell command to invoke dred
    #
    DRED=/Applications/AppleDred.app/Contents/Java/AppleDred.jar
    java -jar $DRED --serving || (open -a AppleDred; sleep 2)
    exec java -jar $DRED "$@" 

There\'s a bug in the present Mac OS X implementation of scrollbars (in
all look-and-feel variants) so (for the moment) we replace the active
scrollbars under Mac OS X with a passive indicator of the position and
relative size of the current view of the document. This indicator shows
up as a narrow bluish rectangle in the right margin of the window.
Vertical and horizontal scrolling can be accomplished with the
scrollwheel and/or by dragging with button-2 pressed. (Remark (May
2017): I confess that I\'m no longer interested in solvig this problem:
it hasn\'t bothered me for a long time).

[]{#Windows (XP) Usage}

Windows (XP) Usage
------------------

*(Warning: written in 2005: now probably obsolete)*\
Put the Dred.jar somewhere sensible, and make a shortcut to it whose
target and start-in fields looks something like this:

    Target:   C:\WINDOWS\system32\javaw.exe -jar "C:\Documents and Settings\sufrin\Desktop\DRED\Dred.jar" 
    Start in: "C:\Documents and Settings\sufrin\Desktop"

Doubleclicking this shortcut starts a new Dred in an editing session
with an anonymous document; dropping a file onto it starts a new Dred in
an editing session for that file.

You may want to put a `dred.bindings` file in the installation directory
once you have found out a bit more about bindings. When you have done
so, add the following text to the shortcut target, just before the -jar
switch:

    -DDREDBINDINGS="C:\Documents and Settings\sufrin\Desktop\DRED\dred.bindings" 

If you really care about economy, then read [Under the
Hood](Underhood.html) for details of how Dred can be set up to use a
single server. It\'s slightly trickier under Windows XP2, which tries
hard to prevent \"Trojan\" ports being opened. The bottom line is that
you should choose port number (\#\#\#\#) above 8000 that you know won\'t
be used by any other server, and change the shortcut to:

    Target:   C:\WINDOWS\system32\javaw.exe -jar "C:\Documents and Settings\sufrin\Desktop\DRED\Dred.jar" --serve==####

[]{#PDFSync}

PDFSync
-------

Dred is compatible with `pdfsync` (at least on OS/X and Linux). On my
Mac I use the Skim pdf viewer, and my `linedisplay` script calls the
Skim `displayline` script to position the viewer at the location
generated from the current text editing position. I have configured Skim
to invoke the following shell script when the mouse is pointed at a
position in the viewer.

    file=$1
    line=$2
    dred --position=$file@$line

The script opens the named `$file` (if there is not already an open
session for it), and positions it at the specified `$line`.

The Latex tool gives the option of using the shell command `linedisplay`
automatically whenever the manuscript file the tool is managing is
latexed.

    # linedisplay script: called from dred's Latex tool with arguments linenumber pdffile texfile -- synctex is needed.
    # echo linedisplay "$@"
    displayline -b "$@"

[]{#The Dred Editing Session Window}

The Dred Editing Session Window
-------------------------------

In each editing session window you will see the main text editing
component at the bottom, with three \"minitext\"-editing components in a
row above it. The minitexts are labelled \"\....\" (pronounced
\"Argument\"), \"Find\" and \"Repl\".

The *Find* and *Replace* minitexts provide arguments for the actions of
the same name. The little checkboxes next to them indicate whether they
are interpreted as \"regular expression\" or \"literal\".

The *Argument* minitext provides arguments for many of the other editing
commands, including those on the *File* menu which start new editing
sessions, allow the document to be saved in a different file, and so
forth.

Many of the editing commands described below are bound to keys, and can
also be used on the minitexts.

Dred shows which of its (mini-)text windows has the keyboard focus by
showing its cursor in red. A text window with a greyed-out cursor
doesn\'t have the keyboard focus. Focus usually moves into a text window
as the mouse-cursor moves into it, but occasionally after popup warnings
it\'s necessary to force the issue by clicking in the desired window.

[]{#Editing Model}

Editing Model
-------------

At any stage, the document in a window (minitext or main) may have a
*selection* (which is a contiguous region of the document). It is
bounded at one end by the *document cursor* \-- which is shown as a thin
red rectangle \-- and at the other end by the *mark* \-- which is shown
as a thin green rectangle. When the cursor and mark are in the same
position only the cursor is shown, and the selection (in that document)
is effectively empty.

The document cursor is moved by the arrow keys. It can be placed
anywhere in the document by clicking with the left mouse-button. The
mark can be placed anywhere in the document by clicking with the right
mouse button.

The selection is always shown on a grey background.

After every editing action the position of the document in the editor
window is adjusted automatically so that *the document cursor is
visible.* Between editing commands the position of the document in the
window can be adjusted manually by the scroll bars, the scrollwheel, or
by middle-dragging and this can make the document cursor disappear
temporarily.

There is a *cut-buffer* which holds at most one text. It is implemented
by the system clipboard, and material placed in the cut buffer is also
available in the clipboard. Material placed in the system clipboard by
another application is accessible through the cut buffer.

[]{#Editing Actions}

Editing Actions
---------------

The editing actions described below are a small selection of the actions
provided by the editor. Brief documentation for *all* the actions, and
indications of their current binding(s) to keys and/or Menu buttons can
be seen by pressing the **Help/Show Bindings** menu item.

(see also [Further documentation of bindings](Bindings.html))

### Cut and Paste

The *Cut* (C-x) action removes the selection from the document and puts
it in the cut-buffer. The *Paste* (C-v) action inserts the content of
the cut-buffer into the document and re-selects it. The *Copy* (C-c)
action replaces the cut-buffer with the selection (if there is a
selection), and the *SwapSel* (C-V) action exchanges the selection and
the cut-buffer (except that if the selection is null, then the
cut-buffer doesn\'t change). The *SwapCursorAndMark* (C-s) action does
what its name suggests.

### Search and Replace

#### Search and Replace Basics

The three minitexts at the top of an editing window are called the
*\....* (or *Argument*), the *Find*, and the *Replace* texts,
respectively.

The *FindDown* (C-f) action selects the next (*FindUp* (C-F) previous)
instance of the pattern specified by the *Find* minitext.

If the checkbox adjacent to the *Find* minitext is checked then the
pattern is interpreted as a (Java-style) regular expression, otherwise
it is interpreted literally.

If the current selection is an instance of the Find text then the
*ReplaceDown* (C-a) (C-r) action saves the current selection in the
cut-buffer and replaces it with the (meaning of the) *Repl* minitext \--
leaving the cursor to the right of the mark. The *ReplaceUp* (C-A) (C-R)
button is analogous and leaves the cursor to the left of the mark in the
replacement text.

The meaning of the *Repl* minitext is the text itself if the checkbox
adjacent to it is NOT checked. Otherwise its meaning is the text
obtained (using standard Java regular-expression substitution rules) by
substituting each instance of `$n` in it by the text that matched the
`n`th bracketed expression in the instance of the pattern that is being
replaced.

The *FindUp* action is usually bound to the SHIFTED key that the
*FindDown* action is bound to; likewise the *ReplaceUp* action is
usually bound to the SHIFTED *ReplaceDown* key.

The *FindSelDown* (C-A-f) action makes the current selection the *Find*
pattern, turns off regular expression interpretation, and then acts as
the *FindDown* action. Likewise *FindSelUp* (C-A-F) makes the current
selection the *Find* text, then acts as the *FindUp* action.

The *ClearArgument* (C-Ins) action moves the keyboard focus into the
*Argument* minitext area and erases the text that is there. *ClearFind*
(Ins) and *ClearRepl* (A-Ins) behave analogously in the *Find* and
*Repl* minitexts.

#### Search and Replace in Practice

One way of replacing the next instance of \"FOO\" with \"BAR\" is to
type the keystrokes bound to

          ClearFind F O O FindSelDown ClearRepl B A R ReplaceDown

Replacing the following instance just requires

          FindSelDown ReplaceDown

since the *Find* and *Repl* minitexts are already set to the right
pattern and replacement.

The *ReplaceAll* action replaces (without any interaction) all instances
of the *Find* minitext within the current selection with the *Repl*
minitext, and selects the resulting transformed text. The original
selection is preserved in the cut buffer, so this action can be undone
with *SwapSel*.

If you want to \"approve\" each replacement interactively, then just use
*FindSelDown* *ReplaceDown* in sequence repeatedly, undoing any
replacement you don\'t approve of with *SwapSel*.

### Treatment of the Selection

Dred offers two modes of treating the selection. Most users will decide
on one of them and stick to it forever. (I use the second, having grown
accustomed to it in my homegrown editors for more than thirty years.)

1.  **Typing-cuts-selection:** Typing new material automatically cuts
    the selection into the cut buffer. This is the behaviour that most
    people have come to expect of editors.\
    In this mode Dred distinguish between three types of selection:
    -   *definite selections*: which are those made \"manually\" by the
        user with the mouse or cursor movement actions.
    -   *tentative selections*: which are those made automatically by
        Dred when its automatic bracket-matching is enabled. A tentative
        selection is shown with a slightly lighter grey background than
        definite selections have.
    -   *mixed selections*: which arise when a tentative selection is
        extended manually. A mixed selection may be shown with a mix of
        backgrounds, or with the same colour background as definite
        selections have.

    \
    Tentative selections are not cut when new characters are typed,
    whereas mixed and definite selections are automatically cut when
    characters are typed.
2.  **Typing-removes-mark:** Typing new characters removes the mark, and
    thus deselects the *selection*, but does not delete the selected
    material. In this mode Dred does not distinguish between tentative
    and definite selections.

The choice between treatments is made using the
**File/Preferences/Typing Removes Selection** checkbox. As with all
other preferences its value is preserved between Dred invocations.

### Undoing and the Cut Ring

Dred has no *general* mechanism for undoing, but all the commands that
operate *by inserting, removing, or transforming the selection* are
individually undoable. (This can feel uncomfortable for people who are
accustomed to hitting *the* undo key whenever they make a mistake or
change their minds about something, but one soon adapts)

For example: when the selection is non-null

            Cut      is undone by Paste 
                    
            SwapSel  is undone by SwapSel

and (whether or not the selection is null)

            Paste    is undone by Cut 

Moreover if the selection is an instance of the Find text then

            Replace  is undone by SwapSel

The *Cut Ring* records the last few pieces of material that found their
way into the cut buffer (the number is adjustable). The cut ring can be
accessed by opening the *Cut Ring Editor*: this is a specialized editor
session that normally shows the content of the cut ring. This editor
session behaves in most respects like an ordinary editing session so
material can be transferred from it to any other text window using
*Copy* and *Paste*. (Of course the Cut Ring Editor doesn\'t itself send
material to the cut ring!)

### The Position Ring

As an *aide-memoire*, whenever a \"big\" movement action is executed the
position at which it was executed is appended to the *cut ring* if it
isn\'t already in the ring. The *Prev Position* action moves the cursor
to the last position in the ring and rotates the ring by one step: the
*Next Position* action is its inverse. For the moment the size of the
ring is fixed at 8.

### Markers

The characters with Unicodes `fff0` \... `fffa` (the \"specials\" range)
are treated as markers and are shown (in red) as single glyphs that 
resemble **(1), (2), \... (9), (10)**). They
are removed from the document as it is saved, but otherwise treated the
same as ordinary characters. They can be used to mark places in the
document to which you may wish to return. Don\'t forget to bind
keystrokes to them (I use `ctrl 1` \... `crtrl 0`). You\'ll find that
the standard extension `MarkTool` provides a menu which makes working
with marks fairly easy. One way of loading this extension is to add the
following bindings to one of your bindings files:

               extension path class:/MarkTool/
               extension load MarkTool                  

### Undoing \"Small\" Changes

Any single character insertion can be undone by the *Delete* action.

When there is a nonempty selection in typing-cuts-selection mode, the
*Delete* action behaves exactly like the *Cut* action, and can therefore
be undone by the *Paste* action. In all other circumstances it removes
the character to the left of the cursor from the document without
affecting the cut buffer. In any case the mark is removed, thereby
deselecting the selection.

The *Swap2* action swaps the two characters immediately to the left of
the cursor without affecting the cut buffer. It undoes itself.

[]{#Mouse Actions}

Mouse Actions
-------------

  ----------------- --------------- --------------------------------------------------------------------------------------------------
  Pressing          Button 1        Positions the text cursor and the mark. Automatic bracket-matching is triggered.
  Pressing          ctrl Button 1   Positions the text cursor but not the mark.
  Pressing          Button 3        Positions the mark but not the text cursor.
  Pressing          ctrl Button 3   Removes the mark and deselects the selection.
  Dragging          Button 1        Positions the text cursor, leaving the mark where the cursor started.
  Dragging          Button 3        Positions the mark, leaving the cursor where it is.
  Double-Clicking   Button 1        Selects the \"word\" under the click. The cursor moves to the end of the word nearest the click.
  Triple-Clicking   Button 1        Selects the line under the click. The cursor moves to the end of the line nearest the click.
  Rotating          Mouse Wheel     Scrolls the view of the document vertically without moving the document cursor.
  Dragging          Button 2        Scrolls the view of the document verticallywithout moving the document cursor.
  ----------------- --------------- --------------------------------------------------------------------------------------------------

[]{#Opening and Saving files}

Opening and Saving files
------------------------

### Saving

When you have finished editing a file, you can save it using the
**File/Save**, or **File/Save As** menu entries or the corresponding
shortcuts. Dred saves a backup copy of the file in a file whose path is
made by appending a tilde \"`~`\" to the path of the original file. If
the \"secondary backups\" preference is enabled, and the single-tilde
backup file exists, then a double-tilde backup file is written.

Dred is quite cautious about saving a file. If it looks like the file
has been written during the period that it has been being edited, then
Dred will warn you and ask you what to do. Likewise, if you
**\"Save As\"** using the name of an existing file, Dred will ask
whether you really mean it.

#### Character Set Encoding

A file is usually saved in the character set encoding that it originated
in. It can be saved in a different encoding by using the
**File/Save As (Browse)** menu entry and changing the encoding using the
encoding selector provided in the file browser.

### Opening

Use the **File/Edit \....** menu entry with the name of the file in the
argument minitext, or use the **File/Edit (browse)** menu entry to start
a new editing session. The browser opens in the current working
directory of the editing session, which is usually that from which the
session was started.

#### Filename Completion

The \"filename completion\" shortcut (usually TAB) in the argument
minitext causes the text in that minitext to change to the longest
common prefix of all the files in the *current scope* that start with
the existing value of the minitext.

1.  If the minitext is empty, then a browser is opened in the current
    working directory (the directory from which the session was
    started).
2.  If the minitext looks like an absolute path then the current scope
    is the parent of that path. For example, the scope of the path `/b`
    is `/`, and the scope of `/b/` is `/b/`
3.  If the minitext starts with `./` then the leading `./` is replaced
    by the absolute path of the current working directory of the editing
    session (which is usually the current working directory directory
    from which the session was started), and rule 2 is applied.
4.  If the minitext starts with `../` then the leading `../` is replaced
    by the absolute path of the parent of the current working directory
    of the editing session, and rule 2 is applied.
5.  In all other cases, the minitext is prefixed with *the parent path
    of the file being edited* in the current session, and rule 2 is
    applied.

[]{#Opening a URL}

#### Opening a URL

Dred will do its best to read a filedenoted by URL, but it cannot yet
*save* documents to locations specified by a URL. If you edit a
URL-specified file, you will need to use **Save As** to save it
somewhere else.\
Protocols known to work (providing no credentials are required for the
file) are `ftp:` and `http:`\
Dred has an additional protocol that gives it access to resources (such
as documentation and pre-designed bindings files) that come packaged in
its .jar file. To access such a file one prefixes its name with `dred:`.
Specific examples that might be useful are: ``

       dred:/dred-bs.bindings
       dred:/function.bindings
       dred:/style.bindings
       dred:/symbols.bindings
       dred:/ctrl.bindings
       dred:/Manual.html
       dred:/Bindings.html
       dred:/Underhood.html
       dred:/index.html
       dred:/MarkTool.java

#### Character Set Encoding

A file may be loaded using a specified encoding by using the
**File/Edit (Browse)** menu entry and changing the encoding using the
encoding selector provided in the file browser. []{#Fonts}

Fonts
-----

Dred usually uses the platform standard font known to Java as
`MONOSPACED 14`. But on some platforms this may lack glyphs for some of
the Unicodes that you need to use. So it is possible to set up Dred to
use a different default font, and it is also possible to change the font
used in a particular session. (Note added in 2012: since this was
written platform-standard fonts have dramatically improved their Unicode
coverage, and I haven\'t needed to use another for about six years).

If the Java system property (or the environment variable) `DREDFONT` is
set, then it is interpreted as the specification (in the standard Java
font specification notation) of a standard Java font, unless it takes
one of the forms

            truetype:url options
            type1:url    options

In that case, the *url* (default protocol `file:`) is assumed to denote
a (truetype or type1) font description file.

The *options* are either empty, or take one of the forms

           
            @pointSize
            @pointSize-shape-options

When no point size is specified, the pointsize is taken to be 14.

The *shape-options* may include up to one of each of the following:

            b[old]          # bold
            i[italic]       # italic

and up to one of:

            m               # the font is deemed to be monospaced (char width is M-width)
            f               # the font is deemed to be monospaced (char width is M-width)
            uXXXX           # the font is deemed to be monospaced (char width is unicode XXXX-width)

The shape options are separated by \"-\".\
Examples of font specifications are

            MONOSPACED-14                                             # 
            Sanserif-bold-14                                          # 
            truetype:arialuni.ttf@14-b                                # bold italic arial unicode from a local font file
            truetype:arialuni.ttf@16.3-b-i-m                          # bold italic M-spaced arial unicode from a local font file
            truetype:file:arialuni.ttf@16.3-b-i-u2167                 # bold italic spaced arial unicode from a local font file
            type1:/usr/X11R6/lib/X11/fonts/Type1/courb.pfa@12-i       # italic courier from an X11 font file

The **View/Monospaced** menu checkbox switches Dred\'s rendering of a
proportionally-spaced font to and from a simulation of that used for a
monospaced font. The radiobuttons below it allow you to select (from a
small number of models) the character whose width is used to determine
the monospace pitch. This can be helpful if you want to work on material
containing glyphs that cannot be found in a monospaced font.

The **View/Set font to \...** and the **View/Set default font to \...**
menu entries change the font used in the current window (or that used in
the current windopw and all future windows) to the font specified in the
Argument (\...) minitext.

The **View/Antialising** checkbox controls the degree of effort Dred
puts into rendering characters. The only time I find antialiased
rendering a little too slow for comfort (on my 1.8mhz PIV) is when
rendering certain heavily hinted Type1 fonts of the kind that come with
Acrobat Reader. Truetype fonts render more than fast enough for my
taste.

[]{#Preferences}

Persistent Preferences
----------------------

The **File/Preferences** submenu displays many of Dred\'s preferences
that persist between Dred invocations. Amongst these are

Typing removes selection (or not)

Tooltips enabled

Secondary backups (saving generates a secondary backup if a primary
backup exists)

Flat Look and Feel (if the slightly lurid Java standard turns you off)

Save Preferences on Exit (if you want preference settings to be
persistent)

The bracket-matching settings on the **Edit** menu, and the
**Antialiasing** setting on the **View** menu are also persistent.

[]{#Etc}

Etc
---

#### Automatic bracket matching

When automatic bracket matching is enabled (I usually enable it), it is
triggered when the cursor is moved (by the mouse) to the left of a left
bracket, (respectively moved to the right of a right bracket, or when
the last character of a right bracket is typed). The mark is then moved
to the right of the corresponding right bracket (respectively to the
left of the corresponding left bracket). Dred\'s interpretation of
\'corresponding\' respects nesting of the triggering bracket, but
ignores others. The built-in correspondences are as follows.

            (               )
            {               }
            [               ]
            <.*>            </.*>
            \begin{.*}      \end{.*}

At present there is no necessary link between the text matching \".\*\"
that appears in an opening LaTeX or XML bracket and that which appears
in the corresponding closing bracket.

The above behavioural \'\'specification\'\' is ambiguous, for it says
nothing about what would happen if the cursor were placed just after the
closing `<h2>` in a text such as

    <a name="Etc"><h2>Etc</h2></a>

In fact the `h2`-tagged text will be chosen.

#### Interrupting long-running tasks

The *KillProcess* action, invoked by pressing the little red button at
the bottom right of the frame, will abort a long-running search or
external process. If you\'re editing an enormous file and have
bracket-matching enabled, then a search for a matching opening or
closing bracket might take a very long time. The stop button\'s
background turns red during a search, but not during execution of an
external process.

#### Unicode

A good starting point for matters Unicode is Markus Kuhn\'s [UTF-8 and
Unicode FAQ for Unix/Linux](http://www.cl.cam.ac.uk/~mgk25/unicode.html)

A Truetype font that implements a very large proprtion of the Unicode
set is Microsoft\'s Arial Unicode. Microsoft no longer distribute this
font, but Googling `arialuni.ttf` is sure to help you find it. Although
it may cost you nothing, this font is not free\.... It also occupies
about 24mb, and *has fewer mathematical symbols then many of the
standard Java fonts*.

#### What needs rewriting?

1.  The core document representation is very robust and efficient, but
    at the cost of making some kinds of search slower than they might
    be, and making multiline searches impossible. One day \....
2.  I lost patience with Java/Swing\'s keyboard focus machinery: my
    replacement uses brute force.

#### What needs more Documenation?

You name it! The Extension API is completely undocumented at present.
Only by reading the source code and the JavaDoc can you \.....

[]{#Brief History}

Brief History
-------------

Dred was written in Java 1.5 during between early February and mid-April
2005 because, having given up trying to maintain a decade-old editor
I\'d written in Python+Tk, I needed a run-anywhere lightweight and
reliable editor that I could use to edit UTF8-encoded Jape theories. I
had tried a number of public domain editors written in Java, but found
that they made editing mathematical texts quite tedious, had very long
startup times, or were insufficiently customizeable for me.

I\'ve been using Dred (on various x86 Linux distros, Solaris, OS/X
(Tiger, Snow Leopard, Lion), and Windows) for all my editing since
mid-May (2005), and have found it completely reliable.

It runs on the (Sun/Oracle) Java 1.5 and later JVMs on all variants of
Linux, Windows, and Solaris on which they can be installed.

Remark (May 2017): I recently implemented `pdfsync` compatibility, and
did some maintenance enabling the editor to run on OS/X with Java
1.\[78\], and to use recent variants of `subversion`. In doing so I
corrected a couple of long-unnnoticed bugs.

    Bernard Sufrin
    $Revision$
    $Date$
    $PrevRevision: 225 $
    $PrevDate: 2012-06-21 17:31:54 +0100 (Thu, 21 Jun 2012) $
    $PrevRevision: 194 $
    $PrevDate: 2007-07-05 18:10:51 +0100 (Thu, 05 Jul 2007) $
