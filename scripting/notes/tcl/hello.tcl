#!/bin/sh
# hello.tcl \
exec tclsh "$0" "$@"

# We do this ^ instead of regular /usr/bin/tclsh
# Why this works -- the \ symbols screens the string in tcl, but not in bash,
# so bin/sh will run the exec on the same file, but tcl will see it as a comment
# Why we do this -- so we don't have to know, where tclsh is on the user system
# Also, we can pass args to tclsh without limit on char count in the shebang
# header. 

# A command to the interp
puts "Hello, world"

# Commands are sep by \n, but can run multiple on one line w/ ;
puts "Hello" ; puts "Goodbye"

# A comment can only be at the start of a command
# puts "?" # comment -- doesn't work
puts "?" ; # comment -- works

# tclsh also has REPL.
# Unlike bash, the interp prints the result of each command (lisp like)
# For example, expr computes an expr and doesn't print anything, but the interp
# will in repl mode

# will print nothing here
expr 5 * 3
expr "5 * 4"
expr "5 * 3" + 15
expr "5 * 3" "+ 5"

# [] allows passing the result of a command to another command
puts [expr 5 * 3]
puts [expr "5 * 4"]
puts [expr "5 * 3" + 15]
puts [expr "5 * 3" "+ 5"]

# exec runs external programs and returns output as result
puts [exec ls]

# Note, that printing is a side effect of ls, and it's result is an empty line
puts [puts "What does puts return?"]

# [] works inside " too
puts "2 * 3 = [expr 2 * 3]"
