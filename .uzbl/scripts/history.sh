#!/bin/sh

file=/home/tree/.uzbl/data/history
[ -d `dirname $file` ] || exit 1
echo `date +'%Y-%m-%d %H:%M:%S'`" $6 $7" >> $file
