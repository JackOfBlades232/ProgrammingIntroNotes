#!/bin/sh
# 12_05/10_21_remake.tcl \
exec wish "$0" -name "12_05/10_21_remake" "$@"

wm title . "12_05/10_21_remake solution"

set buttonHeight 50
set buttonPadding 5
set pixPerLetter 10
set minWindowWidth 0

foreach a $argv {
    set al [string length $a]
    set minWindowWidth \
        [expr $minWindowWidth + 2 * $buttonPadding + $al * $pixPerLetter]
}

. configure -height $buttonHeight -width $minWindowWidth

set i 0
set offset 0
foreach a $argv {
    set al [string length $a]
    set size [expr 2 * $buttonPadding + $al * $pixPerLetter]

    set ids(.b$i) $i
    proc on_click { b } {
        upvar ids($b) id
        puts "$id"
        exit 0
    }

    button .b$i -text $a -relief ridge -padx $buttonPadding \
        -command [list on_click .b$i]

    place .b$i -x $offset -y 0 -width $size -height $buttonHeight
    set offset [expr $offset + $size]
    incr i
}

bind . <Escape> { exit 1 }

after 100 set sleepDone 1 ; vwait sleepDone
wm attributes . -fullscreen 0
wm resizable . no no
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "Error: $result"
}

