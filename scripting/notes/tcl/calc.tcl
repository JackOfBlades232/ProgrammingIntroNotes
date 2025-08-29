#!/bin/sh
# calc.tcl \
exec tclsh "$0" "$@"

# Tcl, unlike lisp, prolog & hope was made with practicality in mind, so
# it is not as hard to write a self-defined user facing program

# For proper handling of stuff, tcl has branching. In a command-scripting
# language this concept is worth discussing.

if { $argc == 1 } {
    # @NOTE: this is still not ok if the arg is not a valid expr
    # puts [expr [lindex $argv 0]]

    # Fixed as such
    # This construction will be explored later
    set x [lindex $argv 0]
    set code [catch { expr $x } result]
    if { $code } {
        puts stderr "Couldn't evaluate expression '$x'"
    } else {
        puts $result
    }
} else {
    puts stderr \
        "Please specify an arithmetic expression as the first arg, such as:"
    puts stderr "    $argv0 '2*2'"
    exit 1
}

# What's new here?
# 1) stderr arg for puts. that's clear.
# 2) == -- also clear. Note that tcl doesn't have = at all
# 3) catch -- deferred for later
# 4) Branching constructions
#
# 4 shall be further looked into
