#!/bin/sh
# 12_05/10_19_remake.tcl \
exec wish "$0" -name "12_05/10_19_remake" "$@"

wm title . "12_05/10_19_remake solution"

if { $argc != 1 } {
    puts stderr "Invalid usage, correct: proc \[message\]"
    exit 1
}

font create f -family Courier -size 12

set minWidth [expr int(12 * [string length [lindex $argv 0]])]
if { $minWidth < 100 } { set minWidth 100 }

label .msg -text [lindex $argv 0] -relief flat -font f
button .byes -text "Yes" -relief ridge -command { exit 0 } -font f
button .bno -text "No" -relief ridge -command { exit 1 } -font f

place .msg -relx 0.5 -rely 0.5 -relheight 0.1 -relwidth 1.0 -anchor s
place .byes -relx 0.5 -rely 0.5 -relheight 0.1 -relwidth 0.3 -anchor ne
place .bno -relx 0.5 -rely 0.5 -relheight 0.1 -relwidth 0.3 -anchor nw

wm minsize . $minWidth 100
wm resizable . no no
