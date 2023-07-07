AppleRed Log and Roadmap
========================

Log
===

Wed 19 Oct 2022 12:11:28 BST

* Branch Version_1.0

  * Fully-functioning.
  
  * Branched from main to mark the start of an experiment
    in using RedScript to read profile/bindings.

    It's my intention to carry forward any non-profile changes
    made on `main` to `Version_1.0`.
        
Fri 11 Nov 2022 21:49:25 GMT

* Tag Version_1.09

  * Fully functioning with RedScript configuration

  * Key-map construction-notation enables a single
    keyboard handler to supplant the previous `orElse`ed
    chain of handlers.

  * `Red` invokes `RedScript` functions defined in the bindings script to
    populate its various menus.

    `RedScript` functions defined in the bindings script have
    access to `SessionCommands` (mostly known by their name).

Roadmap
=======

Short term
----------

Before the end of January 2023 I expect to:

  1. Refactor the UI after introducing the notion of *language
  configuration* -- an assemblage of special-purpose keystrokes
  and menu items that correspond to a particular language or
  formalism.

  2. Rebuild the implementation of `RedScript`, systematising its
  object protocol, and using it more opervasively, so as to simplify
  adding new functionality implemented in Scala, but delivered
  through `RedScript`.

Fri Jul  7 16:51:44 BST 2023

Both the above were completed.

A bug in selection-shading that manifested when the
display was "panned" (as lineshadings not being
wide enough) was fixed today. 


Longer term
-----------

  * Right-to-left scripts are currently out of bounds: this should not
  remain the case. 

  * Admit the colouring of parts of documents to prepare the way
  for syntax highlighting.

  * Experiment with *straightforward* ways of interfacing to the ``Language
    Server Protocol”.

        https://en.wikipedia.org/wiki/Language_Server_Protocol

    I don't yet know what I mean: clearly the services offered are going
    to determine the detail and the complexity of the implementation.
    So, for example, I imagine it will be fairly easy to implement a
    ``go to definition'' keystroke, that'll just open a session
    at the definition. I'm less sure about how to make a light and
    *unobtrusive* interface to (for example) type checker and/or
    method inventory. (I find IntelliJ/Scala somewhat obtrusive).


  
    

