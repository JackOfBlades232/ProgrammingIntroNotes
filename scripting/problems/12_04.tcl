#!/bin/sh
# 12_04.tcl \
exec tclsh "$0" "$@"

set gLongestLine ""

foreach fname $argv {
    if { [catch { set fd [open $fname] } openError] } {
        puts stderr $openError 
        exit 1
    }

    set lLongestLine ""

    while { [gets $fd line] >= 0 } {
        set ll [string length $line]
        if { $ll > [string length $lLongestLine] } {
            set lLongestLine $line
            if { $ll > [string length $gLongestLine] } {
                set gLongestLine $line
            }
        }
    }

    puts "$fname:$lLongestLine"
}

puts "*:$gLongestLine"
