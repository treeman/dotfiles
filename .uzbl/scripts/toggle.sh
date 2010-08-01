#!/bin/sh

show_tablist=$8

if [[ -z "$show_tablist" || $show_tablist -eq 1 ]]; then
    echo "set show_tablist = 0" > $4
else
    echo "set show_tablist = 1" > $4
fi

echo "toggle_status" > $4
