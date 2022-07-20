# Makes an installable AppleRed.app from AppleRed.scpt
# Copies an "improved" INFO.plist into the App
#

all:            AppleRed.app; \
                cp AppleRed.icns AppleRed.app/Contents/Resources/droplet.icns;\
                cp Info.plist    AppleRed.app/Contents/Info.plist

AppleRed.app:   Info.plist AppleRed.scpt; \
                osacompile -o AppleRed.app AppleRed.scpt
                
                
