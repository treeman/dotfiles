#!/bin/sh

file=$HOME/.uzbl/data/bookmarks
[ -r "$file" ] || exit

url=$6

x=0
tmp=""
while [ $x -lt $(wc -l <$file) ]
do
    let x=x+1
    line=`head -n $x $file | tail -n 1`
    test=$(echo $line | cut -d ' ' -f3)

    if [ "$test" != "$url" ]; then
        if [ "$tmp" != "" ] ; then
            tmp="$tmp\n"
        fi
        tmp="$tmp$line"
    fi
done

echo -e $tmp > $file
echo "set uri_color = \"#99FF66\"" > $4
