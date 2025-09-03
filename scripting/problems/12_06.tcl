#!/bin/sh
# 12_06.tcl \
exec wish "$0" -name "12_06" "$@"

wm title . "12_06 solution"

proc invalid_usage_exit {} {
    puts stderr "Invalid args, usage: prog \[-13|-15\]"
    exit 1
}

if { $argc != 1 } { invalid_usage_exit }

set gameArg [lindex $argv 0]
if { $gameArg == "-13" } {
    set labels {0 "00" 1 2 3 4 5 6 7 8 9 10 11 12 13 " "}
    set win1 {1 0 2 3 4 5 6 7 8 9 10 11 12 13 14 15}
} elseif { $gameArg == "-15" } {
    set labels {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 " "}
    set win1 {0 1 2 3 4 5 6 7 8 9 10 11 12 14 13 15}
} else {
    invalid_usage_exit
}

set win2 {0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15}

proc randint {min max} {
    return [expr int(rand() * ($max - $min + 1) + $min)]
}
proc lswap {lsn i j} {
    upvar $lsn ls
    set tmp [lindex $ls $i]
    lset ls $i [lindex $ls $j]
    lset ls $j $tmp
}

set state $win2
while { $state == $win2 } {
    for { set i 0 } { $i < 15 } { incr i } {
         set dest [randint $i 15]
         lswap state $i $dest
    }
}

after 100 set sleepDone 1 ; vwait sleepDone
wm attributes . -fullscreen 0
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "i3 floating toggle error: $result"
}
wm minsize . 100 100
wm maxsize . 1280 1280
wm resizable . yes yes ;

proc lbl { id } {
    global labels
    return [lindex $labels $id]
}

proc upd {id} {
    global state
    set stateId [lindex $state $id]
    if { $stateId == 15 } {
        place forget .fr$id.b
    } else {
        place .fr$id.b -relx 0 -rely 0 -relheight 1 -relwidth 1
        .fr$id.b configure -text [lbl $stateId]
    }
}

proc move {id} {
    global state
    set c1 [expr $id - 4]
    set c2 [expr $id + 4]
    set c3 [expr $id - 1]
    set c4 [expr $id + 1]
    if { $c1 >= 0 && [lindex $state $c1] == 15 } {
        lswap state $id $c1
    } elseif { $c2 >= 0 && [lindex $state $c2] == 15 } {
        lswap state $id $c2
    } elseif { $c3 >= 0 && [lindex $state $c3] == 15 } {
        lswap state $id $c3
    } elseif { $c4 >= 0 && [lindex $state $c4] == 15 } {
        lswap state $id $c4
    }
}

proc checkwin {} {
    global state win1 win2
    if { $state == $win1 || $state == $win2 } {
        for { set i 0 } { $i < 16 } { incr i } { grid forget .fr$i }
        label .winl -text "You've won"
        place .winl -relx 0.5 -rely 0.5 -relheight 1 -relwidth 1 -anchor center
        after 2000 set sleepDone 1 ; vwait sleepDone
        exit 0
    }
}

proc onpress {id} {
    move $id
    for { set i 0 } { $i < 16 } { incr i } { upd $i }
    checkwin
}

set dim 40
proc s { id } {
    global dim state
    set initId [lindex $state $id]
    frame .fr$id -relief flat -height $dim -width $dim
    button .fr$id.b \
        -relief raised -bd 3 -text [lbl $initId] -command [list onpress $id]
    place .fr$id.b -relx 0 -rely 0 -relheight 1 -relwidth 1
    return .fr$id
}

grid [s 0]  [s 1]  [s 2]  [s 3]  -sticky nswe
grid [s 4]  [s 5]  [s 6]  [s 7]  -sticky nswe
grid [s 8]  [s 9]  [s 10] [s 11] -sticky nswe
grid [s 12] [s 13] [s 14] [s 15] -sticky nswe

grid columnconfigure . all -uniform a -weight 1
grid rowconfigure . all -uniform b -weight 1

bind . <Escape> exit
