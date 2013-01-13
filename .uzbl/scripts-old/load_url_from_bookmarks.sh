#!/bin/bash

file=$HOME/.uzbl/data/bookmarks
[ -r "$file" ] || exit

action=$8

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10"
goto=`awk '{print $3}' $file | $DMENU $COLORS`

[ -n "$goto" ] && echo "$action $goto" > $4
