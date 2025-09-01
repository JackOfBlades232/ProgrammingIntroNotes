#!/bin/sh
# placement.tcl \
exec wish "$0" -name "placement" "$@"

wm title . "tcl/tk placement demo"

proc sleep {ms} {
    after $ms set sleepDone 1
    vwait sleepDone
}

# As always with UI, placement, sizes, scaling and all things are important.
# In tk, that task is on a geometry manager.

# Geometry manager is an entity that allows to control how slave widgets
# are placed in relation to master widgets, and how changes to one lead
# to changes to another. It's not always from master to slaves -- sometimes
# geometry manager will change master's characteristics based on change to
# slaves -- this is called geometry propagation.

# There are three geometry managers in tk
# place -- allows setting slave pos/dims rel to master in pixels or coeff fracts
# grid -- sets up a grid inside the master, widgets are assigned cells in it
# pack -- sets up ui 'automatically' from certain directives. Easiest to get
#   smth on the screen, but hard to get a specific layout.
#   and, many times you need to unify elements into a frame/labelframe,
#   and they sometimes behave differently that windows.
#   However, pack is a big win in getting ui-s up an running easily

# also, whatever takes pixel values, also takes other
# for example, -padx 2c is 2 centimiters, 2i is 2 inches, 2m is 2 millimeters,
# 2p -- 2 typographic point1, 1/72 of an inch

label .l1 -text "Label 1" -relief raised
label .l2 -text "Label 2" -relief raised
label .l3 -text "Label 3" -relief raised
label .l4 -text "Label 4" -relief raised

# Let's begin with place
# At pixels
place .l1 -x 10 -y 10 -width 200 -height 100
# at relative offsets
place .l2 -relx 0.5 -rely 0.5 -relwidth 0.1 -relheight 0.1
# With an anchor (which is topleft corner by default, i.e. nw)
place .l3 -relx 0.5 -y 0 -width 150 -height 60 -anchor n
# Combine rel and abs -- they sum up
place .l4 -x 20 -relx 0.5 -y 200 -width 150 -height 60 -anchor n

# there are also helper subcommands
puts ". slaves: [place slaves .]"
puts ".l4 info: [place info .l4]"

sleep 3000

# removes from placement
place forget .l1
place forget .l2
place forget .l3
place forget .l4

# Now, for grid
# Grid fills up the whole master, and has as many rows and columns, as
# have been untilized by widgets.

# -rowspan and -columnspan are 1 by default
grid .l1 -row 0 -column 0 -columnspan 3
grid .l2 -row 7 -column 9 -rowspan 5 -columnspan 10

# In basic config label sizes will stay the same, but rows and columns will
# expand and contract to fit that

grid .l3 -row 0 -column 3 -rowspan 5 -columnspan 10
grid .l4 -row 5 -column 3 -rowspan 2 -columnspan 10

# This is very tedious, and can be done differently
# grid by default can consider a command as a whole line specification
# in this case -row and column are not used at all, and without rowspan
# and columnspan we can use fillers for empty cells.
# x means leave empty, - means stretch last widget to the cell, ^ stretches
# the widget above to the cell
#
# Continued in grid.tcl
