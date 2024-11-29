# Application Profile invoked just before the JVM
# by AppleRed.app as packaged in the orthodox way

#
# Sets up a top-level environment suitable for use by latex, etc.
# This could be done by importing a shell initialization script (but I advise against it)
#
export PATH=$HOME/bin:$HOME/Scala/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/texbin
export TEXINPUTS=.:$HOME/Tex//:
export TEXFONTS=.:$HOME/Tex//:
#
#
#
export HOST=$(hostname -s)
export HOSTNAME=$(hostname -s)
