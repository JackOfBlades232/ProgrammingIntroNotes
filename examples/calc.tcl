#!/bin/sh
# calc.tcl \
exec tclsh "$0" "$@"

if { $argc == 1 } {
    puts [expr [lindex $argv 0]]
} else {
    puts stderr "Please specify an arithmetic expression"
    puts stderr "as the first parameter, such as"
    puts stderr "  $argv0 '2*2'"
    exit 1
}
