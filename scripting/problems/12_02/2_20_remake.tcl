#!/bin/sh
# 12_02/2_20_remake.tcl \
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

while { true } {
    gets stdin line
    if { [eof stdin] } {
        break
    }
    set sl [string length $line]
    for { set i 0 } { $i < $sl } { incr i } {
        set prevIsspace \
            [expr $i == 0 || [isspace [string index $line [expr $i - 1]]]]
        set cur [string index $line $i]
        set curIsspace [expr [isspace $cur]]

        if { $prevIsspace && !$curIsspace } {
            puts -nonewline "("
        } elseif { !$prevIsspace && $curIsspace } {
            puts -nonewline ")"
        }
        puts -nonewline $cur
    }
    if { $sl > 0 && ![isspace [string index $line [expr $sl - 1]]] } {
        puts ")"
    } else {
        puts ""
    }
}
