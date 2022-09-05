Building AppleRed
=================

Building AppleRed.jar in/from the Jed folder
--------------------------------------------

The command

    make

compiles the entire source into Red.jar, then makes from it
an AppleRed.jar that can be run anywhere, on any operating system,
from the scala top-level because it has embedded in it all the
libraries it requires.

The files Makefile, and Makefile-Scala specify various other make
targets, some of which will be of use while working on new features
with IntelliJ.

        ./ired [args]

runs the editor using the compilation classpath and the current IntelliJ-compiled  class files

        ./sred [args]

runs the editor using the compilation classpath and Red.jar 

You can test the new variant you have compiled with the script 

        ./ired [args]

You can specify the logging of classes/modules by providing
additional arguments of the form

        -l<package>.<class>

which logs at or below the most detailed level; or

        -l<package>.<class>=<level>

which logs at or below the specified level.
    
For example:

        ./ired -lRed.EditSession=FINER exampletexts/L03.tex

generates this log as the first few mouse clicks are made


      INFO@Logging.Default     :
    **********
    AppleRed starting at 2022-09-05-182803
    **********
      INFO@Logging.Default  : Red.EditSession=FINER
     FINER@Red.EditSession  : theSession.setMark(254)
     FINER@Red.EditSession  : selectUntil 254 -> 254
     FINER@Red.EditSession  : setCursorAndMark(12, 35) with selection=Span(254,254,false)
     FINER@Red.EditSession  : theSession.setMark(240)
     FINER@Red.EditSession  : selectUntil 240 -> 240
     FINER@Red.EditSession  : setCursorAndMark(12, 21) with selection=Span(240,240,false)
     FINER@Red.EditSession  : theSession.setMark(246)
     FINER@Red.EditSession  : selectUntil 235 -> 246

When you are happy with the program, you can take steps to package it; there
are details for doing this in the folder `RedApp`.