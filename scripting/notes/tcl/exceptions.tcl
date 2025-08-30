#!/bin/sh
# exceptions.tcl \
exec tclsh "$0" "$@"

# Along with commands returning values, there are special commands that
# stop the execution of another. return for procs, an cycle bodies and
# scripts called via source, break for cycles and continue for cycle iterations
#
# There is also errors, which stops the command stack until a catch, or
# it stops the script.
#
# As has been said before, this works due to commands returning not only the
# result, but also a return code, an integer.
# Codes are: ok (0), error (1), return (2), break (3), continue (4)
#
# These codes control the flow of the interpreter. Most commands return ok(0),
# which just means go on. All other codes mandate stopping execution of
# this 'level of invocation' (not necessarily the frame, as loop/cond bodies
# are not frames in the sense of vars)
#
# On a nonzero code, the interpreter will walk levels until it finds one
# that has a handler for said code. All cycles put up handlers of 3 and 4,
# that's why break and continue work. An invocation of a proc-d command
# and the main script have a 2 handler, so return works everywhere.
#
# 1 is only handled by our catch. Also, if you call break or continue not
# in a loop, any return handler will catch it, and pass on as 1.
# On the other hand, return passes through break and continue handlers.
# 
# The return command allows to specify the code returned by the proc
# (not by the return command itself! it will return code 2)

proc my_break {} { return -code 3 }
proc my_continue {} { return -code 4 }
# or, for return with -code we have special string vals
proc my_break {} { return -code break }
proc my_continue {} { return -code continue  }

# these work just like break/continue
# For return, we can now make a procedure that acts as return and generates
# a value

proc retABC {} { return -code 2 ABC }

proc p { x } {
    if { $x == 1 } {
        retABC
    } else {
        return {}
    }
}

puts [p 1]

# Now, the catch command runs a script, stores the result and returns the code
# Quite simple
#

proc sus { x } {
    if { $x == 0 } {
        break ; # converts to 1
    }
    if { $x == 1 } {
        return -code 3 "Huh" ; # actual way to get a numbered signal
    }
    if { $x == 2 } {
        error "Faild"
    }
    return "Ok"
}

proc run { x } {
    set c [catch { sus $x } res]
    if { $c == 1 } {
        puts stderr "Error: <<$res>>"
    } else {
        if { $c == 0 } {
            puts "Ok; result is $res"
        } else {
            puts "Terminated with code $c; <<$res>>"
        }
    }
}

run 0
run 1
run 2
run 3
