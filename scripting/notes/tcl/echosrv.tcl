#!/bin/sh
# basics.tcl \
exec tclsh "$0" "$@"

# let's make an echoserver
# @NOTE: not handling sigint, cause it's not native

set clientsToServe 5

proc verify args {
    set code [catch { eval $args } result]
    if { $code != 0 } {
        puts "Error: $result"
        exit $code
    }
    return $result
}

proc process {channel addr port} {
    global sessionsClosed done clientsToServe

    if { [gets $channel line] >= 0 } {
        puts $channel $line
        flush $channel
    }
    if { [eof $channel] } {
        close $channel
        incr sessionsClosed
        if { $sessionsClosed >= $clientsToServe } { set done 1 }

        set left [expr $clientsToServe - $sessionsClosed]
        puts "$addr:$port closed connection, $left to go"
    }
}

proc accept {channel addr port} {
    global sessionCount clientsToServe
    if { $sessionCount >= $clientsToServe } {
        puts stderr \
            "Can't accept $addr:$port, over limit of $clientsToServe sessions"
        close $channel
        return
    }
    incr sessionCount

    puts stderr "$addr:$port connected"
    fconfigure $channel -blocking false
    fileevent $channel readable [list process $channel $addr $port]
}

set server [verify socket -server accept 4221]
set done 0
set sessionCount 0
set sessionsClosed 0

while { !$done } {
    vwait done ; # processes all events, i. e. accept and all process-es
}

puts "All clients served!"
close $server
