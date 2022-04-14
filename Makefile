#
#          Make this main program as a script dependent on
#          external libraries and the scala code runner.
#
MAIN   := Jedi
JAR    := $(MAIN).jar
PKG    := Jed

#
# location of the IntelliJ-compiled class files
#
IJOUT    := ./out/production/Jed
#
# Name of the script that uses IntelliJ-compiled class files
#
IJMAIN := i$(MAIN)

#
#
include Makefile-Scala
#
#  

