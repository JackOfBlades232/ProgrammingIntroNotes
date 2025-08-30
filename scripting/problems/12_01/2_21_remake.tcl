#!/bin/sh
# 12_01/2_21_remake.tcl \
exec tclsh "$0" "$@"

proc isspace {c} {
    if { $c == " " } {
        return true
    } elseif { $c == "\t" } {
        return true
    } elseif { $c == "\n" } {
        return true
    } elseif { $c == "\r" } {
        return true
    } else {
        return false
    }
}

set prev " "
set accum ""

while { true } {
    set cur [read stdin 1]
    if { [eof stdin] } {
        break
    }
    if { ![isspace $prev] && [isspace $cur] } {
        if { [string length $accum] == 2 } {
            puts -nonewline $accum
        }
        set accum ""
    } 
    if { ![isspace $cur] } {
        set accum $accum$cur
    } else {
        puts -nonewline $cur
    }
    set prev $cur
}

if { ![isspace $prev] } {
    if { [string length $accum] == 2 } {
        puts $accum
    } else {
        puts ""
    }
}
