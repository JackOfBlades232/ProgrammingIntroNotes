#!/bin/sh
# widgets.tcl \
exec wish "$0" -name "widgets" "$@"

wm title . "tcl/tk widgets demo"

# We've already seen toplevel windows (toplevel), buttons (button) and
# labels (label)
#
# Tk also supports
# checkboxes (checkbutton)
# choice buttons (radiobutton)
# string entry fields (entry)
# text editors (text)
# frames for grouping other widgets (frame)
# frames with label (labelframe)
# choice lists (listbox)
# menus (menu)
# buttons for bringing up the menu (menubutton)
# formatted text messages (message)
# scales for choosing a value in an interval (scale)
# multi-pane windows with movable borders (panedwindow)
# scrollers (scrollbar)
# windows for geometric drawings (canvas)
# and more
#
# can be read about in man 3tk ...

# Two main interaction ways are:
# passing args on creation
# using the widget name as a command
#
# Common subcommand on all widgets are cget and configure.
# There are options supported by some of them, and some only by one
# those that are supported by > 1 can be seen in man 3tk options

# One of the options is -text. Labels, buttons, labelframes etc have it, but
# toplevel windows and labelless windows don't
# There is also variant -textvar, which takes a gvar name, and 
# the text will react to value change (via tcl trace command)

set okText "Ok"
button .ok -textvar okText -command exit
pack .ok -side bottom -fill x
label .okl -textvar okText
pack .okl -side bottom -fill x

# For entry textvar is getting changed on value change
entry .input -textvar okText
pack .input -side bottom -fill x

# One other common option: -relief , for frame of the widget
# vals: raised/sunken/solid/groove/ridge/flat
# For frame width -- -bd
.okl configure -bd 3 -relief ridge
.ok configure -relief sunken

# Many widgets support padx/pady
# Some (like button and label) use it only for text
.ok configure -padx 20 -pady 20
# some, like windows -- for all contents
. configure -padx 50 -pady 50

# canvas, for example, doesn't have these opts at all

# Most widgets are passive. That means, nothing happens to them unless
# invoked by the tcl/tk program. Label is such a widget.
# Actually, we can override that, but still.
#
# Other widgets, like button, entry, etc -- are active, and made to react to
# user events, and they have special options to set that up.

# All active widget have the -state option. For different widgets different
# states are available, but this allows disabling widgets.

# checkbutton tracks a variable state. For additional commands,
# it has select/deselect/toggle
#
# When setting up, it either creates an implicit gvar by the last token of
# the name, or it can be explicitly specified by -variable option
# by default on-value is 1 and off-value is 0, but it can be customized with
# -onvalue and -offvalue
# -indicatoron is an option to add/remove the checkbox -- if it's set to 0,
# it's just a button that gets 'pressed' and 'unpressed'
checkbutton .disable -text "Disable input" -variable inputDisabled -command {
    if { $inputDisabled } {
        .ok configure -state disabled
        .input configure -state readonly
    } else {
        .ok configure -state normal
        .input configure -state normal
    }
}
pack .disable -side bottom -fill x

# radiobutton has a bunch of stuff that checkbutton has, except toggle,
# to create a bunch of radiobuttons in one group, we use -variable to
# tie them to one variable and -value to give each one a designated value
radiobutton .choiceA -text "Choose A" -variable chosen -value "A"
radiobutton .choiceB -text "Choose B" -variable chosen -value "B"
radiobutton .choiceC -text "Choose C" -variable chosen -value "C"

pack .choiceA -side top -fill x
pack .choiceB -side top -fill x
pack .choiceC -side top -fill x

label .choiceDisplay -textvar chosen
pack .choiceDisplay -side top -fill x

# entry widget also allows commands get, delete and insert
puts [.input get]
.input insert 0 "Some text"
.input delete 8 end
# end is special value for end of string

# it also has icursor to change the cursor pos, selection for change selected
# text, and a bunch of other stuff emulating the work state of a human with
# the widget

# As can be seen above, for entry the binding is through -textvar, not
# -variable. That's just how it is.
#
# With -validate and -validatecommand options one can set up input validation
# on the form
#
# There is a bunch more stuff, but this is fine
