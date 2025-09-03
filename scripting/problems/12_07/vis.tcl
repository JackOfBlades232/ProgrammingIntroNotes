#!/bin/sh
# 12_07/vis.tcl \
exec wish "$0" -name "12_07" "$@"

wm title . "12_07 solution"

proc invalid_usage_exit {} {
    puts stderr "Invalid args, usage: prog N M | N, M are ints, N <= M"
    exit 1
}

if { $argc != 2 } { invalid_usage_exit }

set n [lindex $argv 0]
set m [lindex $argv 1]
if { ![string is int $n] || ![string is int $m] } { invalid_usage_exit }
if { $n > $m } { invalid_usage_exit }
if {
    $n <= 2 ||
    ($n <= 3 && ($m == 3 || $m == 5 || $m == 6)) ||
    ($n == 4 && $m == 4)
} {
    puts stderr "No open or closed solutions exist for an $n x $m board"
    exit 2
}

set dim 40

update
wm attributes . -fullscreen 0
wm resizable . no no ;
wm maxsize . [winfo screenwidth .] [winfo screenheight .]
wm geometry . [format "%dx%d" [expr $dim * $m] [expr $dim * $n]]
wm protocol . WM_DELETE_WINDOW exit
update
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "i3 floating toggle error: $result"
}

frame .grid -relief flat
place .grid -relx 0 -rely 0 -relheight 1 -relwidth 1

proc s {id r t} {
    global dim
    frame .grid.fr$id -relief $r -bd 3 -height $dim -width $dim
    label .grid.fr$id.lb -text $t
    place .grid.fr$id.lb -relx 0.5 -rely 0.5 -anchor center
    return .grid.fr$id
}
proc bs {id} { return [s $id sunken ""] }
proc fs {id nm} { return [s $id flat $nm] }
proc cs {id} { return [s $id flat ""] }

set alphabet {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z}

set gid 0
grid [cs 0] -row 0 -column 0 -rowspan 1 -columnspan 1 -sticky nswe ; incr gid
for { set i 1 } { $i <= $m } { incr i ; incr gid } {
    set l [lindex $alphabet [expr ($i - 1) % [llength $alphabet]]]
    grid [fs $gid $l] -row 0 -column $i -rowspan 1 -columnspan 1 -sticky nswe
}
for { set i 1 } { $i <= $n } { incr i ; incr gid } {
    grid [fs $gid $i] -row $i -column 0 -rowspan 1 -columnspan 1 -sticky nswe
}

for { set i 1 } { $i <= $m } { incr i } {
    for { set j 1 } { $j <= $n } { incr j ; incr gid } {
        grid [bs $gid] -row $j -column $i -rowspan 1 -columnspan 1 -sticky nswe
    }
}

set lm [expr $m + 1]
set ln [expr $n + 1]
for { set i 0 } { $i <= $ln } { incr i ; incr gid } {
    grid [cs $gid] -row $i -column $lm -rowspan 1 -columnspan 1 -sticky nswe
}
for { set i 0 } { $i < $lm } { incr i ; incr gid } {
    grid [cs $gid] -row $ln -column $i -rowspan 1 -columnspan 1 -sticky nswe
}

grid columnconfigure .grid all -uniform a -weight 1
grid rowconfigure .grid all -uniform b -weight 1

bind . <Escape> exit

proc open_read {fname} {
    if { [catch { set fd [open $fname] } openError] } {
        puts stderr $openError 
        return false
    }
    return $fd
}
proc file_is_ok {f} {
    if { $f != false } {
        return true
    } else {
        return false
    }
}

set tourp [open_read "|12_07/tour.pl $n $m"]
if { ![file_is_ok $tourp] } {
    puts stderr "Failed to run solver, make sure to run from the problems dir"
    exit 2
}

# @NOTE: we could validate what comes from the process. However, this is
# our bundle, and we should rather make sure that the combo is correct
# and the 2 scripts come together.

set path {}

proc xy_to_wid {x y} {
    global m n
    return [expr 1 + $m + $n + $x * $n + $y]
}

proc draw {x y d} {
    global path m n
    set wid [xy_to_wid $x $y]
    set c [expr double($d + 1) / double($m * $n)]
    set r [expr int((1.0 - $c) * 255)]
    set g [expr int($c * 255)]
    set b 0
    .grid.fr$wid.lb configure -fg [format "#%02x%02x%02x" $r $g $b] -text "$d"
}

proc clear {x y} {
    global path
    set wid [xy_to_wid $x $y]
    .grid.fr$wid.lb configure -text ""
}

set lastUpdate 0

while { [gets $tourp line] >= 0 } {
    set d [lindex $line 0]
    set y [expr [lindex $line 1] - 1]
    set x [expr [lindex $line 2] - 1]

    if { $d < [llength $path] } {
        for { set i $d } { $i < [llength $path] } { incr i } {
            set e [lindex $path $i]
            set xx [lindex $e 0]
            set yy [lindex $e 1]
            clear $xx $yy
        }
        set path [lreplace $path $d end]
    }

    lappend path "$x $y $d"

    foreach e $path {
        set xx [lindex $e 0]
        set yy [lindex $e 1]
        set d [lindex $e 2]
        draw $xx $yy $d
    }

    set now [clock milliseconds]
    if { [expr $now - $lastUpdate] >= 1 } {
        update
        set lastUpdate $now
    }
}

close $tourp
