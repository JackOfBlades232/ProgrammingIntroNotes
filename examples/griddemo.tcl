#!/bin/sh
# griddemo.tcl  // this program is hereby put into public domain
# The next line starts wish \
exec wish "$0" -name "grid_demo" "$@"

wm title . "tcl/tk grid demo"

set dim 40

proc m { id } {
    global dim
    frame .fr$id -relief sunken -bd 3 -height $dim -width $dim
    label .fr$id.lb -text "$id"
    place .fr$id.lb -relx 0.5 -rely 0.5 -anchor center
    return .fr$id
}

grid [m a1] [m b1] [m c1] [m d1] [m e1] [m f1] [m g1] [m h1] -sticky nswe
grid [m a2] x      [m c2] x      x      [m f2] [m g2] [m h2] -sticky nswe
grid [m a3] [m b3] -      -      [m e3] -      -      -      -sticky nswe
grid [m a4] [m b4] [m c4] [m d4] [m e4] [m f4] [m g4] [m h4] -sticky nswe
grid [m a5] ^      [m c5] [m d5] [m e5] ^      [m g5] [m h5] -sticky nswe
grid [m a6] x      [m c6] [m d6] [m e6] ^      [m g6] [m h6] -sticky nswe
grid [m a7] [m b7] [m c7] x      x      [m f7] [m g7] [m h7] -sticky nswe
grid [m a8] [m b8] [m c8] x      x      [m f8] [m g8] [m h8] -sticky nswe

grid [button .b -text Close -command exit -bd 4] \
  -row 6 -column 3 -rowspan 2 -columnspan 2

grid columnconfigure . all -weight 1 -uniform a
grid rowconfigure . all -weight 1 -uniform b

bind . <Escape> exit
