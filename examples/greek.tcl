#!/bin/sh
# \
exec tclsh "$0" "$@"

set greek {
   alpha beta gamma delta epsilon zeta eta
   theta iota kappa lambda mu nu xi omicron
   pi rho sigma tau upsilon phi chi psi omega
}
foreach { x y z } $greek {
   puts "$x $y $z"
}
