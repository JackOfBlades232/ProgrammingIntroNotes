#!/bin/sh
# io.tcl \
exec tclsh "$0" "$@"

# io is kinda weird in tcl. One of the reasons is binary data -- everything
# is a string in tcl, but bindata has to be handled in some way.
#
# Opening an io stream is done via open. It can open a file, or launch a proc
# and attach itself to in/out of the process.
#
# It takes 1 to 3 args. open name [mode] [perms]
#
# modes can be like fopen modes -- r/w/a/r+ and optional b for binary
# It can also be a list of
# RDONLY/WRONLY/RDRW/APPEND/BINARY/CREAT/EXCL/NOCTTY/NONBLOCK/TRUNC ,
# which are the same as, in turn, for open
#
# the third param is, as in crt/unix, an octal perms code, 0666 by default
#

# read by default
set er [open "exceptions.tcl"]

# same
# set a [open "a" "w"]
# set b [open "b" {WRONLY CREAT TRUNC}]

# tcl file descriptors are also strings (fileN)
puts $er

# close closes. For blocking io it waits for all output to flush,
# for nonblocking -- weird shit, we basically have to serve it up.
close $er

# The opens above are not safe. This would be more correct.

proc open_read {fname} {
    if { [catch { set fd [open $fname] } openError] } {
        puts stderr $openError 
        return false
    }
    return $fd
}
proc open_write {fname} {
    if { [catch { set fd [open $fname "w"] } openError] } {
        puts stderr $openError 
        return false
    }
    return $fd
}
proc file_is_ok {f} {
    if { $f != false } {
        return true
    } else {
        return false
    }
}

set f [open_read "basics.tcl"]
set nf [open_read "alsjdha.tcl"]

# It's quite useless to use process io with bidir (r+ or w+), because pipes
# don't get flushed on newlines, so there will be no dialogue (that's why
# when I do process output marshalling through pipes, I always fflush),
# and we can't make sure that the launched process does that. We can do
# that on our side via flush

# For outputting to file, we use puts with fd. stdin, stdout and stderr
# are special fd values (seen in prev examples)

set wf [open_write "test"]
if { [file_is_ok $wf] } {
    # For puts we can disable newline at the end with -nonewline
    puts $wf "Hellof, file!"
    puts -nonewline $wf "EOF"
    close $wf
}

# for input we have read and gets. Unlike puts, it requires a stream,
# for std input use stdin

# if a file name starts with |, it will be treated as a process to launch &
# create pipes according to mode. close for processes waits for them to finish,
# which is sadge, cause we can't kill em

set ls [open_read "|/bin/ls"]
if { [file_is_ok $ls] } {
    # gets reads until newline or eof
    # has 2 forms. 1-arg -- returns line. 2-arg -- stores line and returns
    # char count or -1, like the c functions
    puts "First line: [gets $ls]"
    while { [gets $ls line] > 0 } { puts "Line: $line" }
    close $ls
}
set grep [open_write "|grep -v abc"]
if { [file_is_ok $grep] } {
    puts $grep "Hi"
    puts $grep "abc"
    puts $grep "?"
    puts $grep "Hi abc"
    close $grep
}

# One can also use the one-arg ver of gets. On eof, it gives empty line,
# and we can use eof to check if last op raised a eof (pascal style)

if { [file_is_ok $f] } {
    while { ![eof $f] } { puts [gets $f] }
    close $f
}

# The second command -- read, reads N symbols in read f N form,
# and reads all file otherwise. If it's blocking, it'll block, otherwise
# will give erryting it has at that moment

puts [read stdin 5]

# will block until eof
# puts [read stdin]

# like eof, there is also the fblocked command which can check if the stream
# gave less bytes than we wanted it to

set nbf [open_read "procs.tcl"]
if { [file_is_ok $nbf] } {
    # fconfigure for settings on a stream, here instead of NONBLOCK flag
    fconfigure $nbf -blocking false
    while { ![eof $nbf] } {
        set buf [read $nbf 100]
        if { [fblocked $nbf] } {
            puts "blocked on io!"
        }
        puts $buf
    }
    close $nbf
    # if we had buffered output, by the docs it would be flushed and 
    # the strea would be closed in passive mode, but in reality we'd
    # need to serve it in a vwait call
}

# One last thing -- close can also take read/write or r/w to close only one
# dir if the stream is bidir (2 pipes)

# there is also
# binary --
#   format/scan for converting string repro of bindata for binary streams
#   encode/decode [base64/uuencode/hex] for converting to/from encodings
# file -- all the stuff for file system interaction
