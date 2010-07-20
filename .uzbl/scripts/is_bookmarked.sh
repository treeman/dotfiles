#!/bin/sh

file=$HOME/.uzbl/data/bookmarks
[ -r "$file" ] || exit

url=$6

x=0
while [ $x -lt $(wc -l <$file) ]
do
    let x=x+1
    line=`head -n $x $file | tail -n 1`
    test=$(echo $line | cut -d ' ' -f3)

    if [ "$test" = "$url" ]; then
        echo "set uri_color = \"#FFF826\"" > $4
        exit
    fi
done
