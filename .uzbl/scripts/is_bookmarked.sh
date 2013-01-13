#!/bin/sh

# Update uri color pending on bookmark status

UZBL_BOOKMARKS_FILE=$HOME/.uzbl/data/bookmarks

>> "$UZBL_BOOKMARKS_FILE" || exit 1

x=0
while [ $x -lt $(wc -l <$UZBL_BOOKMARKS_FILE) ]
do
    let x=x+1
    line=`head -n $x $UZBL_BOOKMARKS_FILE | tail -n 1`
    uri=$(echo $line | cut -d ' ' -f1)

    if [ "$uri" = "$UZBL_URI" ]; then
        echo "set uri_color = @uri_bm_color" > "$UZBL_FIFO"
        exit
    fi
done

echo "set uri_color = @uri_no_bm_color" > "$UZBL_FIFO"

