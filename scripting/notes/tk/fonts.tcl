#!/bin/sh
# fonts.tcl \
exec wish "$0" -name "fonts" "$@"

wm title . "tcl/tk fonts demo"

# Any widget can display text in only one font
# For that, all text-displaying widgets have the option -font
# The font option takes a list as information

label .l -text {This is a lbl} -font {Courier 20 italic underline}
pack .l -side top -fill none

# Tk guarantees three fonts on every platform: Times, Helvetica & Courier
# The additional options can be:
# normal, bold, roman, italic, underline, overstrike
# The size is typographic points

# This way is tedious. There is a way to create fonts to then use like that
font create myFont -family Helvetica -size 30 -slant roman -weight bold
label .l2 -text {Another lbl!} -font myFont
pack .l2 -side top -fill none

# This is also good because changing this font will change all using widgets
label .l3 -text {And another lbl} -font myFont
pack .l3 -side top -fill none

after 1000 set sleepDone 1 ; vwait sleepDone
font configure myFont -size 15
