#!/bin/sh
# x11hello.tcl \
exec wish "$0" -name "x11hello" "$@"

wm title . "tcl/tk x11 echo demo"
label .lb -text "$argv0 $argv" -padx 10 -pady 10
pack .lb -side top -fill x
button .ok -text "Ok" -command exit
pack .ok -side bottom -fill x
