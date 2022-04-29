#!/bin/sh
# packdemo.tcl  // this program is hereby put into public domain
# The next line starts wish \
exec wish "$0" -name "pack_demo" "$@"

wm title . "tcl/tk pack demo"

. configure -height 300 -width 300
pack propagate . 0

set i 0
for { set n 0 } { $n < 5 } { incr n } {
    foreach sd { top left bottom right } {
        incr i
        label .l$i -text $i -relief ridge -bd 3
        pack .l$i -side $sd -fill both -expand 1
    }
}

button .b -text Close -command exit -bd 4
pack .b -side left -expand 1 -fill both

bind . <Escape> exit
