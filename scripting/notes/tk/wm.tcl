#!/bin/sh
# wm.tcl \
exec wish "$0" -name "wm" "$@"

proc sleep {ms} {
    after $ms set sleepDone 1
    vwait sleepDone
}

wm title . "wm demo"
label .lb -text "$argv0 $argv" -padx 10 -pady 10
pack .lb -side top -fill x

# There are a bunch of other things wm command can do that relate to window
# manager interactions

# set window to fullscreen
wm attributes . -fullscreen 1

# change window hidden/minimized/etc state
sleep 200
wm state . withdrawn
sleep 200
wm state . iconic
sleep 200
wm state . normal

# @NOTE: iconic is like normal on i3, as it doesn't have icons

# wm state without param returns the status
puts ". is [wm state .]"

# same as state changes but as commands
sleep 200
wm withdraw .
sleep 200
wm iconify .
sleep 200
wm deiconify .

# difference is, these command work for windows that have not been shown
# yet (state won't)

# For icon control use
# iconbitmap, iconmask, iconname, iconphoto, iconposition, iconwindow
# Won't test cause don't have icons

# wm geometry changes position & shape of window on the screen

# make i3 comply
wm attributes . -fullscreen 0
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "Error: $result"
}

sleep 50
wm geometry . 200x300+50+70 ; # both parts are optional

# It can also position itself relative to other border of the scree
sleep 400
wm geometry . 250x340-50-50
sleep 400
wm geometry . -100-100

# we can constrain the window sizes and make it resizable
wm minsize . 100 150
wm maxsize . 1280 720
wm resizable . yes yes ; # yes for w and h

# One can set window to be gridded with wm grid / -setgrid
# With it window's size and all is in grid increments
#
# There are a bunch of other things it can do, but not very important --
# for involved gui apps tcl/tk isn't the best idea
