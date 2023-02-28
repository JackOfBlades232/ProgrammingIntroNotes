#!/bin/sh
# 5_7/time_test.sh

CMD=$*
TIME=$( date +%s )
$CMD
STATUS=$?
NEWTIME=$( date +%s )
echo $(( NEWTIME - TIME ))
exit $STATUS
