# 
# Makes an installable $(APP).app from $(APP).scpt
#
#   Essential: Injects an "improved" Info.plist $(APP).plist into  $(APP).app
#   Essential: Injects $(APP).icons into $(APP).app as droplet.icns
#
# For a self-contained OS/X $(APP).app that can also be used on Linux
#
#   Essential: copies the $(APP).jar into $(JAVA) 
#   Essential: copies the $(APP)-server script into $(JAVA) 
#   Essential: copies the $(APP) icons into $(RESOURCES) 
#   Essential: copies the $(APP) Shellscripts and Profiles to $(RESOURCES) 
# 
# The $(APP).app directory can be installed either on OS/X or on Linux
# by copying it to ~/Applications. Before use is essential to copy the
# three Profiles files to your $(HOME) and adjust them to your own 
# requirements. The following command will do the copy. The profiles
# are nearly identical -- the important things to change are the
# PATHs exported from .AppleRed-application.profile; the other profiles
# are (but needn't be) essentially the same.
# 
# 
#     cp -pr $(find ~/Applications/AppleRed.app -name ".AppleRed*.profile") ~
#
# The following commands will run from anywhere:
#
#     $(find ~/Applications/AppleRed.app -name red) [paths]
#     $(find ~/Applications/AppleRed.app -name cred) [paths]
#
# The former starts an editing server (if it hasn't already started)
# to starting editing sessions for the documents denoted in the filestore
# by [paths]. The server runs until the last of its editing sessions 
# is closed; but keeps running in this case if it was started with
# an empty [paths].
#
# The latter starts its editing sessions independently of any server. 
# It terminates when its last editing session is closed.
#
# The shell scripts function on both operating systems, independently 
# of the AppleRed app being used. On OS/X that app acts as a drop-
# target, or as an open-with subject. When invoked, AppleRed starts a
# editing server (if necessary), then passes its arguments to it, then
# (finally) terminates. Think of it as a "start button" for the server
# and don't expect it to hang around between "starts".
# 
APP=AppleRed
CONTAINER=${APP}.app/Contents
JAVA=${CONTAINER}/Java
RESOURCES=${CONTAINER}/Resources
#
#
#
all:            ; @echo "=============== Ensure you have an up to date $(APP).app in .."; \
                make app install

app:            $(APP)-server $(APP).app ../$(APP.jar) ; \
                cp $(APP).icns          $(RESOURCES)/droplet.icns; \
                cp $(APP).plist         $(CONTAINER)/Info.plist; \
                mkdir -p                $(JAVA); \
                cp -p ../$(APP).jar     $(JAVA); \
                cp -pr Profiles         $(RESOURCES); \
                cp -pr Shellscripts     $(RESOURCES); \
                chmod +x $(APP)-server; \
                cp -p $(APP)-server     $(JAVA); \
                cp -p AppleRed.png      $(RESOURCES); \
                cp -p AppleCred.png     $(RESOURCES); \
                touch app


$(APP).app:     $(APP).plist    $(APP).scpt; \
                osacompile -o   $(APP).app $(APP).scpt

linuxapp:       ; mkdir -p $(APP).app/Contents/Resources
                
install:        app; \
                rm -rf ~/Applications/$(APP).app; \
                cp -pr $(APP).app ~/Applications

spartacus:      app; \
                rsync -av ~/Applications/$(APP).app/ spartacus.local:Applications/$(APP).app/;\
                rsync -av ~/.red/ spartacus.local:.red/

clean:          ; rm -rf app $(APP).app
