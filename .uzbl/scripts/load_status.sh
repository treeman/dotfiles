#!/bin/sh

file=$HOME/.uzbl/status
fifo=$4

if [ -r $file ]; then
    show_tablist=$(echo `cat $file` |cut -d',' -f1)
    show_status=$(echo `cat $file` |cut -d',' -f2)
else
    show_tablist=1;
    show_status=1;

    echo "1,1" > $file;
fi;

echo "set show_tablist = $show_tablist" > $fifo;
echo "set show_status = $show_status" > $fifo;
