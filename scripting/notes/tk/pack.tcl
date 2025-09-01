#!/bin/sh
# pack.tcl \
exec wish "$0" -name "pack" "$@"

wm title . "tcl/tk pack demo"

# pack works quite simply -- it puts the next widget along one of the 4
# sides of the remaining empty space (cavity).
# Initially it's the same as the full window, but then gets distributed
# along top/bottom/left/right sides.
#
# -side choses the side
# -fill tells if the widget should be stretched (but the allocd space always is)
#   takes x y none both
# -expand sets up if the widget should expand if after all the placement some
#   space is left -- pack will distribute the space along all widgets with
#   -expand 1
#
# Note: -fill is for widget inside space, -expand is for the space itself

# example: spiral of widgets

# by default grid and pack set window size themselves (geom propagation)
# but we can override it
# but we can override it
# but for effect, propagation has to be disabled 
pack propagate . 0
. configure -height 800 -width 800

# must go before creating widgets

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

# of course, pack also has slaves/info/forget

after 100 set sleepDone 1 ; vwait sleepDone
wm attributes . -fullscreen 0
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "Error: $result"
}

