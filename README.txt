Building Jedi

** Building Jedi.jar in/from the Jed folder

    make

This makes a jar that can be run anywhere, on any operating system, 
from the scala top-level. The files Makefile, and makeBin specify
various other make targets, including doc, clean, etc. The latter
sets a class path variable, CP to include the compiled scala swing library, 
scala-swing.jar

You can test the new variant you have compiled with the script 

    ./Jedi <arguments>
 
You will need to provide at least one filename argument for the
program UI to be started; though the named file doesn't yet have
to exist. 

You can set specify the logging of classes/modules by providing
arguments of the form

    -l<package>.<class>             (logs at or below the most detailed level)
    -l<package>.<class>=<level>     (log at or below the specified level)
    
For example:

 ./Jedi -lJed.EditSession=ALL foo
 
generates the following log as the first few mouseclicks
are made
    
     INFO@Logging.Default     : Jed.EditSession=ALL
     FINER@Jed.EditSession        : theSession.setCursor(0, 15)->15 (NoSelection)
     FINER@Jed.EditSession        : cursor=15 // notifying (0, 15)
     FINER@Jed.EditSession        : theSession.setCursor(1, 33)->33 (NoSelection)
     FINER@Jed.EditSession        : cursor=33 // notifying (0, 33)
     FINER@Jed.EditSession        : cursor=34 // notifying (1, 0)   
 
and the following command 

./Jedi -lJed.EditSession=FINE -lRed.DocumentView foo

generates a more extensive log, because the DocumentView module 
has a lot to do

      INFO@Logging.Default     : Jed.EditSession=FINE
      INFO@Logging.Default     : Red.DocumentView=ALL
     FINER@Red.DocumentView       : 24 x 79
     FINER@Red.DocumentView       : 1 x 28
     FINER@Red.DocumentView       : 1 x 28
    FINEST@Red.DocumentView       : cursor=(0,0)
      INFO@Red.DocumentView       : wheel -1 0
      INFO@Red.DocumentView       : wheel -2 0
      INFO@Red.DocumentView       : wheel -8 0
      INFO@Red.DocumentView       : wheel -13 0
    FINEST@Red.DocumentView       : cursorToDocument(java.awt.Point[x=146,y=21]) => 0, 8)
     FINER@Red.DocumentView       : CursorChanged: (0, 8)
    FINEST@Red.DocumentView       : cursor=(0,8)
    FINEST@Red.DocumentView       : cursorToDocument(java.awt.Point[x=146,y=21]) => 0, 8)
    FINEST@Red.DocumentView       : cursorToDocument(java.awt.Point[x=146,y=21]) => 0, 8)
      FINE@Jed.EditSession        : deleting .
 