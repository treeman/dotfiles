#!/bin/bash

file=$HOME/.uzbl/data/history
[ -r "$file" ] || exit

action=$8

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10"
goto=`sort -k3 < $file | uniq -f2 | sort -r | awk '{print $3}' | $DMENU $COLORS`

[ -n "$goto" ] && echo "$action $goto" > $4