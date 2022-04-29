#!/bin/bash
# x11echo.tcl  // this program is hereby put into public domain
# The next line starts wish \
exec wish "$0" -name "x11_echo" "$@"

wm title . "tcl/tk x11 echo demo"

label .lb -text "$argv0 $argv ($argc)" -padx 10 -pady 10
pack .lb -side top -fill x

button .ok -text "Ok" -command exit
pack .ok -side bottom -fill x


while { 1 == 1 } {}
