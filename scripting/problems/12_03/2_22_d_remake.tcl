#!/bin/sh
# 12_03/2_22_d_remake.tcl \
exec tclsh "$0" "$@"

proc isdigit {c} {
    if {
        $c == "0" || $c == "1" || $c == "2" || $c == "3" || $c == "4" ||
        $c == "5" || $c == "6" || $c == "7" || $c == "8" || $c == "9"
    } {
        return true
    } else {
        return false
    }
}

foreach a $argv {
    set isDigits true
    for { set i 0 } { $i < [string length $a] } { incr i } {
        if { ![isdigit [string index $a $i]] } {
            set isDigits false
            break
        }
    }
    if { $isDigits } {
        puts $a
    }
}
