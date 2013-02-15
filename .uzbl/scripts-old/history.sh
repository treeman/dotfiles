#!/bin/sh

file=/home/tree/.uzbl/data/history
[ -d `dirname $file` ] || exit 1

if [ -n "$6" ]; then
    echo `date +'%Y-%m-%d %H:%M:%S'`" $6 $7" >> $file
fi
