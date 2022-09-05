Universal AppleRed Application
==============================

Introduction
------------

The "Universal" `AppleRed.app` is a folder/application that can
safely be used on both Linux and OS/X, and can be installed on
either without a fuss. On OS/X it functions as well as an "orthodox"
OS/X app (_ie_ it can be used to open files with the `open` command
or act as a drop target). Its only difference with an orthodox OS/X
app is that it splits its functions into two parts, one of which
acts as an editing service provider for all instantiations of the
app: each invocation of the app passes its arguments to the service
provider over a UDP port.

The "orthodox" OS/X app (instructions for building/installing are
elsewhere) acts as its own editing service, and relies on OS/X to
ensure that there is no more than one such service running at the
same time. Sadly, at present some of the features that are needed by
`pdfsync` cannot be implemented in it because of (perfectly-legitimate)
Apple-imposed security requirements that it would be tedious to
try and meet.

How to build and install it (from OS/X)
---------------------------------------

  1. Build `AppleRed.jar` in the parent folder by: `make AppleRed.jar`
  2. Build `AppleRed.app` in this folder by: `make clean app`
  3. (optional: OS/X or Linux) Install `AppleRed.app` in your local `~/Applications`
  folder by: `make install`
  4. (optional: OS/X or Linux) Install all the scripts from the
  application 
  `~/Applications/AppleRed.app/Contents/Resources/Shellscripts`
  in your local OS/X or Linux scripts folder. This is
  often named `~/bin/` but needn't be. Installing may be done by copying or
  by making (symbolic or other) links.
  5. Copy the various `.AppleRed`...`.profile` files to you home folder.
  You may wish to edit them to establish a richer environment or to
  select a different editing service port number.

How to build and install it (Linux)
-----------------------------------
The "universality" is heavily compromised, of course, unless
you have a way of compiling the Applescript program
`AppleRed.scpt` and generating a "skeleton" `AppleRed.app`
structure into which the Linux components can be "poured".

The `Makefile` target `linuxapp` does the right thing:

        make clean linuxapp app

But the resulting `AppleRed.app` is now suitable only for deployment
on a Linux system.






