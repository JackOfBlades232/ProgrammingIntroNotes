#!/bin/sh
# 12_02/2_21_remake.tcl \
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
    set wordStart 0
    for { set i 0 } { $i < $sl } { incr i } {
        set prevIsspace \
            [expr $i == 0 || [isspace [string index $line [expr $i - 1]]]]
        set curIsspace [expr [isspace [string index $line $i]]]

        if { $prevIsspace && !$curIsspace } {
            set wordStart $i
        } elseif { !$prevIsspace && $curIsspace } {
            if { $i - $wordStart == 2 } {
                puts -nonewline [string range $line $wordStart [expr $i - 1]]
            }
        }

        if { $curIsspace } {
            puts -nonewline [string index $line $i]
        }
    }
    if { $sl > 0 && ![isspace [string index $line [expr $sl - 1]]] } {
        if { $sl - $wordStart == 2 } {
            puts -nonewline [string range $line $wordStart [expr $sl - 1]]
        }
    }
    puts ""
}

