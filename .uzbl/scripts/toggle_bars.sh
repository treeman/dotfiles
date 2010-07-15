#!/bin/sh

file=$HOME/.uzbl/data/status
fifo=$4

toggle_tablist=$8
toggle_status=$9

[ -w $file ] || exit 1

show_tablist=$(echo `cat $file` |cut -d',' -f1)
show_status=$(echo `cat $file` |cut -d',' -f2)

if [ $toggle_tablist == 1 ]; then
    if [ $show_tablist == 1 ]; then
        show_tablist=0
    else
        show_tablist=1
    fi
fi

if [ $toggle_status == 1 ]; then
    if [ $show_status == 1 ]; then
        show_status=0
    else
        show_status=1
    fi
fi

echo "$show_tablist,$show_status" > $file
echo "set show_tablist = $show_tablist" > $fifo
echo "set show_status = $show_status" > $fifo
