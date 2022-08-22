#!/bin/sh
# 1_12-13.sh

CMD=$*
TIME=$( date +%s )
$CMD
STATUS=$?
NEWTIME=$( date +%s )
echo $(( NEWTIME - TIME ))
exit $STATUS
