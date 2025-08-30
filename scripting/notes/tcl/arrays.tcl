#!/bin/sh
# procs.tcl \
exec tclsh "$0" "$@"

# even though everything is a string in tcl, the interp cheats a bit for
# perf. This way, for arrays the interp keeps helper data structures
# for quick addressing.

# There are also associative arrays, which are akin to implicit
# hash tables in the var space

# They are instroduced by setting vars with complex names (no spaces!)
set foo(bar,baz) "abracadabra"

# The point of this is computing indices

proc compute1 {} { return "bar" }
proc compute2 {} { return "baz" }

puts $foo([compute1],[compute2])

# We could in theory implement this ourselves with name pasting

proc getvt {name args} {
    set vn [join [concat $name $args]]
    uplevel 1 "set tablevn_ \"$vn\" ; set vn_ $name ; set vargs_ \"$args\" ; " {
        if { [info exists "$tablevn_"] } {
            upvar 0 "$tablevn_" var
            return $var
        } else {
            error "Value for $vn_\[$vargs_\] doesn't exist"
        }
    }
}
proc setvt {value name args} {
    set vn [join [concat $name $args]]
    upvar $vn var
    set var $value
}

setvt "abracadabra" foo bar baz
puts [getvt foo bar baz]
# puts [getvt foo baz]

# But it's a bit uncool, and in my impl reserves 3 names
# And, most definitely slow as fuck

# we can use arrays as such
proc arr {a i j} {
    upvar $a an
    return $an($i,$j)
}

set foo(b,b) schwabra
puts [arr foo b b]

# we can also address arrays through the array command

# all keys with names
puts [array names foo]
# all kv pairs
puts [array get foo]
# set with a get-formatted list
array set foo {k,p op j,r ock}
puts [array get foo]

# Arrays can't be viewed as data types -- more like an organization
# of the vars 'database' (attributed to this call frame)

# Later versions also have dicts, which are similar, but are actual data types
