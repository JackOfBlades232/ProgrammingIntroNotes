#!/bin/sh
# basics.tcl \
exec wish "$0" -name "basics" "$@"

# wish is an augmented tcl interpreter with the tk library and that also
# reads X variables. It opens a main window on startup

# wm command is the window manager stuff command
# wm title sets new name
#
# . is the name of the main window in the app
# All windows and widgets are in a parent/child and master/slave relationship,
# which usually coincide, but not always.
#
# Parent/child relationship controls destruction (destroying parent destroys
# all children), and naming -- main window is . , all children of X
# are named X.childTag , where child tag is any camel case identifier

wm title . "Basic app"

# This creates a button bt which is a child of . 
# It is not anywhere from this command alone
button .bt -text "Press me"

# This is obviously not a good way of placing stuff, but we can now place
# the button at absolute offsets into the window
place .bt -x 10 -y 10

# Each widget itself becomes a command after introduced.
# On it we can do cget and configure

# cget get the value of a property
set btText [.bt cget -text]
puts ".bt text is $btText"

# a little util
proc sleep {ms} {
    after $ms set sleepDone 1
    vwait sleepDone
}

sleep 200

# configure sets it
.bt configure -text "Press me\nSeriously"
.bt configure -padx 0 -pady 0

sleep 400

# buttons also have flash (play flash anim) and invoke (do as if pressed)
.bt flash

sleep 400

# There are also labels
label .lab -text "This is a label\nNon informative one"
# pack will set it up relative to windows dims
pack .lab -side bottom -fill x

# labels also have cget & configure, as does the main window

. configure -bg #ff00ff

# The main reasons why buttons exist are callbacks, set through configure
# Takes a script-string

set i 0

.bt configure -command {
    incr i
    .lab configure -text "Button has been pressed $i times"
}

# The commands seem to be processed in vwait, and when the script is
# done wish seems to be serving commands until the window closes.
# If we do a manual 'vwait forever', window closing won't stop wish
