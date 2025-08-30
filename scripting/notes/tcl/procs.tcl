#!/bin/sh
# procs.tcl \
exec tclsh "$0" "$@"

# Subprograms in tcl are called procedures, but they are more like custom
# commands. The command proc introduces a new command into the interp,
# which is written in tcl.

# works like: name list_of_args body

proc hello {} {
    puts "Hello, world"
}

# To hammer it home, it's the same as
# Actually same-name redef work silently, so it is just
# a command that changes the interp state
proc "hello" "" "puts \"Hello, world!\""

hello

# The arg list communicates both param count, and how they
# will be named in the body
proc greet {name} {
    puts "Hello, $name"
}

greet Petr
greet {Andrey Viktorovich}

# A proc either returns what last command returned, or explicitly
# via return command

proc hello_ok {} {
    puts "Hello"
    return "Ok"
}
proc quad {x} { expr $x * $x }

puts [hello_ok]
puts [quad 4]

# Tcl allows for variadic funcs with magic arg string 'args'

proc sumargs args {
    set s 0
    foreach a $args {
        set s [expr $s + $a]
    }
    return $s ; # set s would've also worked
}

puts [sumargs 1 2 3]
puts [sumargs 123 12 31 23 12 3 21 3231]
puts [sumargs]

# To set a min on arg count, specify some of them
proc max {x args} {
    set max $x
    foreach a $args {
        if { $a > $max } {
            set max $a
        }
    }
    return $max
}

puts [max 1 2 3]
puts [max 123 12 31 23 12 3 21 3231]
# puts [max]

# Now about var visibility
# In tcl, any var referred to inside of a proc is considered local
# So referencing a global won't work, and setting same-name var won't
# conflict with global vars

set a 1
proc get_a {} {
    set a 2
    return $a
}
puts [get_a]
puts $a

# To get globals, they have to be explicitly brought in (like in python)
proc print_a {} {
    global a
    puts $a
}

print_a
set a 9
print_a

# Since strings are just strings and all is strings, we can pass a
# variable param to global

proc print_and_clear {varname} {
    global $varname
    eval "puts \$$varname"
    set $varname ""
}

print_and_clear a
print_and_clear a

# A more sane and powerful way of doing this is upvar, which brings a var
# from enclosing context into the one (not only for globals, but for locals
# from other procs as well) via a synonym name (like refs in c++)

proc print_and_clear {varname} {
    upvar $varname var ; # can take multiple pairs of args
    puts $var
    set var ""
}

set b "abra"
print_and_clear b
print_and_clear b

proc local_demo {} {
    set c "cadabra"
    print_and_clear c
    print_and_clear c
}

local_demo

# a good example is a variable value swapper
proc swap {a b} {
    upvar $a p $b q
    set t $p
    set p $q
    set q $t
    return {}
}

set x 42 ; set y 36
puts "x=$x, y=$y"
swap x y
puts "swapped, x=$x, y=$y"

# upvar can also take a first arg -- to not take from the caller,
# but explicitly set, how many call frames to go up to look for the var
# N, where N is 1, 2, ... -- go N up (no args -- same as 1)
# #N, where N is 0, 1 ... -- abs id of frame, where #0 is global
# There is also the uplevel command, which handles frame arg same as upvar,
# but runs all other args as scripts in the context of the chosen frame

proc run_at {scr frm} {
    if { [string index $frm 0] == "#" } {
        uplevel $frm $scr
        return {}
    } else {
        uplevel [expr $frm + 1] $scr
    }
}

proc do_nested {n depth} {
    for { set i 0 } { $i < $n } { incr i } {
        run_at { puts $i } $depth 
        if { $depth < 3 } {
            do_nested $n [expr $depth + 1]
        }
    }
}

# Always prints the I from top level call
do_nested 3 0

# Since set implicitly creates var on first set, it can be removed via unset.
# It can be checked via info with param exists. info is a versatile command
# for checking interp state

proc checkvar {name} {
    uplevel 1 "set vn $name ;" {
        if { [info exists $vn] } {
            upvar 0 $vn var
            puts "$vn=$var"
        } else {
            puts "$vn doesn't exist"
        }

        # @NOTE: we do pollute scope with vn
    }
}

set x 49
checkvar x
unset x
checkvar x

