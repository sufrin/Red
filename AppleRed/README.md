# Making `AppleRed.app` for `OS/X` 

This directory has the means to construct a standalone `AppleRed`
app that *can* function without an external server on versions
of `OS/X` earlier than `Catalina`.

Providing that the parent directory has an up-to-date `AppleRed.jar`
constructed either as an **IntelliJ** artefact, or by using the
script `makeApp` in that folder, one of the following commands will
build an app:

        ant clean app   # builds AppleRed.app
        ant clean new   # builds AppleRed-new.app (for testing)
        
The resulting app can be moved or copied to `/Applications` or to
`/Users/`*yourname*`/Applications`. 

### `Catalina`
Pre-Catalina these apps act as clients of the underlying editing
server that they themselves contain. That server can also be used
from shell scripts, such as the included `red` and `tred`.  The
apps can be used as the target of drag-and-drop, and can also be
used from the `Finder` (and its surrogates) to open existing files.

On `Catalina` the situation is complicated by (welcome, but ferocious)
Apple security measures that *forbid* the reception of messages (of
any kind) by uncertified apps. At the time of writing I see no
reasonable way round these measures (and, believe me, I have looked)
that can be taken without paying the Apple Ransom: which imposes both a
financial and an intellectual burden. 

Nevertheless, providing that the `red` script has been installed 
somewhere, and has been started (with or without arguments) 
the `AppleDred` app can use *its* editing server. The command
sequence invocation:

  --
  `red; open AppleDred`
  --
         
starts the server and the app more or less simultaneously. 

None of this is needed if you are happy just using `red` as
a command-line program for editing, as you would have to be
on `Linux`. The following description assumes that there is no
`red` server running. The command-line invocation:
  
  --
  `red` *paths*
  --
   
starts an internal server that starts an editing session for
each of the files at *paths*, and terminates when the last
such session closes. Once the internal server has been started, 
subsequent invocations of `red` use *that* server to start
subsequent sessions.  The command-line invocation:
  
  --
  `red`
  --
   
(without arguments) just starts a server for use by subsequent
`red` invocations. Its behaviour is slightly different: *it keeps
running until it is explicitly quit*.


 








