#!/bin/bash

file=$HOME/.uzbl/data/history
[ -r "$file" ] || exit

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10"
goto=`sort -k3 < $file | uniq -f2 | awk '{print $3}' | $DMENU $COLORS`

[ -n "$goto" ] && echo "uri $goto" > $4
