#!/bin/sh
# 12_03/2_23_remake.tcl \
exec tclsh "$0" "$@"

proc isspace {c} {
    if { $c == " " || $c == "\t" || $c == "\n" || $c == "\r" } {
        return true
    } else {
        return false
    }
}

if { $argc != 1 } {
    puts stderr "Invalid usage, use as: prog \"arg line\""
    exit 1
}

set line [lindex $argv 0]
set len [string length $line]
set count 0

for { set i 1 } { $i < $len } { incr i } {
    if {
        ![isspace [string index $line [expr $i - 1]]] &&
        [isspace [string index $line $i]]
    } {
        incr count
    }
}

if { ![isspace [string index $line [expr $len - 1]]] } {
    incr count
}

puts $count

