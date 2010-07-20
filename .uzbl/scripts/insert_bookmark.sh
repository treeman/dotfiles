#!/bin/sh

file=/home/tree/.uzbl/data/bookmarks
[ -d `dirname $file` ] || exit 1

url=$6
title=$7

echo `date +'%Y-%m-%d %H:%M:%S'`" $url \"$title\"" >> $file
sort -k3 $file | uniq -f2 | sort > $file
echo "set uri_color = \"#FFF826\"" > $4
