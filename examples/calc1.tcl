#!/bin/sh
# calc1.tcl \
exec tclsh "$0" "$@"

if { $argc == 1 } {
    set x [lindex $argv 0]
    set code [catch { expr $x } result]
    if { $code } {
        puts stderr "Couldn't evaluate the expression ``$x''"
    } else {
        puts $result
    }
} else {
    puts stderr "Please specify an arithmetic expression"
    puts stderr "as the first parameter, such as"
    puts stderr "  $argv0 '2*2'"
    exit 1
}
