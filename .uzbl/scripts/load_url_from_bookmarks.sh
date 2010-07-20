#!/bin/bash

file=$HOME/.uzbl/data/bookmarks
[ -r "$file" ] || exit

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10"
goto=`awk '{print $3}' $file | $DMENU $COLORS`

[ -n "$goto" ] && echo "uri $goto" > $4
