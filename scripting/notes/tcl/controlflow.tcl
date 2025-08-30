#!/bin/sh
# controlflow.tcl \
exec tclsh "$0" "$@"

#
# In TCL anything inside a paired {} is just a string!
#
puts {Here starts the experimental output part}
# Difference from "" is that all symbols mean themselves, no screening or $
puts {\\ "" $result}

# The only 'special meaning' is attributed to {} inside {} -- they are still
# themselves, but are counted to identify the end of this particular string
# one can screen a { or a } with \, but they's still be printed

puts {\{}
puts {\}}

# So if is not an operator. Tcl doesn't have operators. It is a command!
# And condition and body are arguments. Cond is evald as if it were expr arg,
# and if it evals to true, then body is interpreted. If there is another
# arg -- else, then another body arg is required, which will be evald in
# the case of cond evaluating to false. (so else is an arg of if)
#
# Now about boolean values. In tcl 0 is false, and fractional zero (0.0) is too,
# anything else (any other number string) -- true
# Also we can use strings true/false, yes/no and on/off, and their prefices
# You can also use these words with any case (in general tcl is case sensitive)
#
# Fun fact: else is not required

if { no } {
    puts "Here"
} {
    puts "There"
}

# we could also put then after cond
if { on } then {
    puts "Then"
}

# To finally desroy the c magic
set b off ; if $b "puts YES" "puts NO"
# The stuff in "" runs just as if it were in {}
# But, of course, we use {} for special symbols not evaluating and nesting

# if also has a result -- which is the result of the last ran command in the
# chosen branch
set b 100
set tt [if $b "puts YES ; expr 10" "puts NO ; expr 20"]
puts "result is $tt"

# Kinda like lisp but these are not forms being computed, but commands
# being executed

# While works in a similar fashion. On each iter the cond expr is re-evald
set n 0
while { $n < $argc } {
    puts "param $n is [lindex $argv $n]"
    set n [expr $n + 1]
}

# Note that if we had used "$n < $argc" in the head, the values of
# n and argc would've been substituted once on string 'creation', and
# while would be infinite. But here, the expr can be re-evald each time

# For is very c-like. It has 4 params: init, cond, step and body,
# evaluated/interpreted as expected (init, step & body -- interp), cond -- eval
for { set n 0 } { $n < $argc } { set n [expr $n + 1] } {
    puts "Parameter $n is [lindex $argv $n]"
}

# a shorthand for inc
for { set n 0 } { $n < $argc } { incr n } {
    puts "Parameter $n is [lindex $argv $n]"
}

# for list (which is a string w/ tokens) iteration we have foreach
# which has 3 args in base form -- var name, list and body
foreach a $argv {
    puts "We've got $a as a command line arg"
}

# If arg 1 is a list, we can process list in batches of n
foreach { a b c } $argv {
    puts "$a $b $c"
}

# and we can zip-iterate multiple lists
foreach a $argv b { 1 2 3 4 } {
    puts "$a $b"
}

# these forms can be arbitrarily combined

# all cycle commands return an empty line, like puts
# there is also switch, not gonna describe here

# break and continue commands break out/skip iter of the cycle
# They work by returning a special code from the interpretation of the block.
# Actually every command has both a result and a completion code, that
# is what catch gets (see calc)

foreach a $argv b { 1 2 3 4 } {
    if { $b == 2 } { continue }
    if { $b == 4 } { break }
    puts "$a $b"
}
