#!/bin/sh
# resources.tcl \
exec wish "$0" -name "resources" "$@"

wm title . "tcl/tk Resources demo"

# Let's look at how to localize the program via X Window resources

# an X-resource is just a name and a string value
# capitalized names are considered classes, others -- names
# The first name in the id is the class, the last is an option,
# the ones in the middle can be window class, window name, etc
# The value is separated by ": "
# for example
#
# Fig.msg_form.balloon_toggle.topShadowContrast: -40
#
# Here in an app of class Fig a window by the name of msg_form and inside
# of it a subwindow balloon_toggle has an option topShadowContrast with the
# value -40.
#
# The names can also have wildcards
#
# Fig*canvas.background: gray97
# means that all canvases will have option background of gray97 and
# Fig*foreground: black
# means black foreground for all widgets in the app Fig
#
# Now, how to use these in tcl/tk
#
# Here the app name is set by the -name param ^
# Now we can set options for widgets via X-resource strings

# Now we can specify text for it via
# resources.dlg1.buttongrp.b1.text: Button One
# and
# reources*Button*.close.text: Close

# or read from a resource file, which is the way for localization
# (resource files have to be in utf8)
if { [lsearch -exact $argv "-ru"] != -1 } {
    option readfile "resources_ru.rc"
} else {
    option readfile "resources_en.rc"
}
# we can add this from the program with option add
# @HUH: why does *Button* break it?
option add "Resources*close.text" "<<"

toplevel .dlg1
frame .dlg1.buttongrp
button .dlg1.buttongrp.b1 -relief ridge -command { puts "Button pressed" }
place .dlg1.buttongrp -relx 0.1 -rely 0.1 -relwidth 0.8 -relheight 0.8
place .dlg1.buttongrp.b1 -relx 0.1 -rely 0.1 -relwidth 0.8 -relheight 0.8

button .close -relief ridge -command exit
place .close -relx 0.1 -rely 0.1 -relwidth 0.8 -relheight 0.8

