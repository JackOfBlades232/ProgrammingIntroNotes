#!/bin/sh
# 12_01/2_20_remake.tcl \
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

while { true } {
    set cur [read stdin 1]
    if { [eof stdin] } {
        break
    }
    if { [isspace $prev] && ![isspace $cur] } {
        puts -nonewline "("
    } elseif { ![isspace $prev] && [isspace $cur] } {
        puts -nonewline ")"
    }
    puts -nonewline $cur
    set prev $cur
}

if { ![isspace $prev] } {
    puts ")"
}
