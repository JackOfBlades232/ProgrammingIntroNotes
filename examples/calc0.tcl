#!/bin/sh
# calc0.tcl \
exec tclsh "$0" "$@"
puts [expr [lindex $argv 0]]
