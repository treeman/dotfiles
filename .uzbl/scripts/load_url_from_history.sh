#!/bin/bash

file=$HOME/.uzbl/data/history
[ -r "$file" ] || exit

file=$(sort -k3 $file | uniq -f2 | sort)
echo -e $file

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10"
goto=`awk '{print $3}' $file | $DMENU $COLORS`

[ -n "$goto" ] && echo "uri $goto" > $4
