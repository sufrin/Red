# 
# Makes   an installable $(APP).app from $(APP).scpt
# Injects an "improved" plist $(APP).plist and other resources into  $(APP).app
#
APP=AppleRed
CONTAINER=${APP}.app/Contents
#
#
#
all:            app install

app:            $(APP).app; \
                cp $(APP).icns          $(CONTAINER)/Resources/droplet.icns; \
                cp $(APP).plist         $(CONTAINER)/Info.plist; \
                mkdir -p                $(CONTAINER)/Java; \
                cp ../$(APP).jar        $(CONTAINER)/Java; \
                chmod +x $(APP)-server; \
                cp -p $(APP)-server     $(CONTAINER)/Java; \
                cp -p AppleRed.png      $(CONTAINER)/Resources; \
                cp -p AppleCred.png     $(CONTAINER)/Resources                


$(APP).app:     $(APP).plist    $(APP).scpt; \
                osacompile -o   $(APP).app $(APP).scpt
                
install:        ; cp -pr $(APP).app ~/Applications

                
                
