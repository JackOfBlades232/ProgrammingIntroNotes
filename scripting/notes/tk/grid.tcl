#!/bin/sh
# grid.tcl \
exec wish "$0" -name "grid" "$@"

wm title . "tcl/tk grid demo"
proc sleep {ms} {
    after $ms set sleepDone 1
    vwait sleepDone
}

# For resizing
sleep 100 ; # let wm open the window to have a recepient for the i3 command
wm attributes . -fullscreen 0
if { [catch { exec i3-msg floating toggle } result] } {
    puts stderr "Error: $result"
}
wm minsize . 100 150
wm maxsize . 1280 720
wm resizable . yes yes ;


# First, lets make a helper to make frames with a label in the middle

set dim 40
proc m { id } {
    global dim
    frame .fr$id -relief sunken -bd 3 -height $dim -width $dim
    label .fr$id.lb -text "$id"
    place .fr$id.lb -relx 0.5 -rely 0.5 -anchor center
    return .fr$id
}

# Now with the grid rowwise command mode we can compose a mosaic in a grid
# By default when widgets occupy more grid cells with an already established
# size, then their own size, they are placed in the center
# For that, the -sticky flag with sides makes the widget stretch to sides
# We'll use it for all 4
grid [m a1] [m b1] [m c1] [m d1] [m e1] [m f1] [m g1] [m h1] -sticky nswe
grid [m a2] x      [m c2] x      x      [m f2] [m g2] [m h2] -sticky nswe
grid [m a3] [m b3] -      -      [m e3] -      -      -      -sticky nswe
grid [m a4] [m b4] [m c4] [m d4] [m e4] [m f4] [m g4] [m h4] -sticky nswe
grid [m a5] ^      [m c5] [m d5] ^      [m f5] [m g5] [m h5] -sticky nswe
grid [m a6] x      [m c6] [m d6] ^      [m f6] [m g6] [m h6] -sticky nswe
grid [m a7] [m b7] [m c7] x      x      [m f7] [m g7] [m h7] -sticky nswe
grid [m a8] [m b8] [m c8] x      x      [m f8] [m g8] [m h8] -sticky nswe

# and a close button, sits at the center of a 2x2 space dictated by the griddef
grid [button .b -text Close -command exit -bd 4] \
    -row 6 -column 3 -rowspan 2 -columnspan 2 -sticky nswe

# In base config we won't scale -- we are tied to the base sizes dictated by
# frame dimensions

# all means that all rows columns are attributed to a uniform group a/b,
# and each is assigned a weight of 1, i.e. that they all can stretch and
# contract, but have to do be the same
grid columnconfigure . all -uniform a -weight 1
grid rowconfigure . all -uniform b -weight 1

# There is a bunch more stuff in columnconfigure and rowconfigure -- they
# are the key to making grid not suck, and there is also more stuff about grid
#
# It also has info/forget/slaves like place
# There is also remove, which removes the widget, but does not remove it's
# influence on the grid
sleep 2000
grid remove .frh1
grid remove .frh2
grid remove .frh4
grid remove .frh5
grid remove .frh6
grid remove .frh7
grid remove .frh8

# Finally, to pack in pack.tcl
