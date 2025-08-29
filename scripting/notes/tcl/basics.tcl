#!/bin/sh
# basics.tcl \
exec tclsh "$0" "$@"

# In tcl almost any string works as an identifier, which can id vars & funcs

# set commands sets a value to a var
set x 25
set str "abra cadabra"

# getting a value out of a var is done w/ $
set n [expr $x + 1]
puts $n
puts $str
# also works in quotes too
puts "Value of n is $n"

# set returns the new val
puts [set n [expr $n + 1]]
# set without args just returns cur val, like $
puts [set n]

# Tcl allows working with a string as a list of tokens
set tokens "a \"b c\" c"
puts [lindex $tokens 1] ; # quotes make b c one token, but lindex discards them

# args are available through
# argc -- string of the number of args without the prog name
# argv -- args without the prog name
# argv0 -- args with the prog name
puts $argc
puts $argv
puts $argv0
