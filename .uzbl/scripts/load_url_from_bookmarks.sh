#!/bin/sh

#NOTE: it's the job of the script that inserts bookmarks to make sure there are no dupes.

UZBL_BOOKMARKS_FILE=$HOME/.uzbl/data/bookmarks

COLORS=" -fn monospace -nb #303030 -nf khaki -sb #CCFFAA -sf #303030"
DMENU="dmenu -i -b -l 10 $COLORS"

[ -r "$UZBL_BOOKMARKS_FILE" ] || exit 1

if [ -z "$DMENU_HAS_VERTICAL" ]; then
    # because they are all after each other, just show the url, not their tags.
    goto="$( awk '{ print $1 }' "$UZBL_BOOKMARKS_FILE" | $DMENU )"
else
    # show tags as well
    goto="$( $DMENU < "$UZBL_BOOKMARKS_FILE" | cut -d ' ' -f 1 )"
fi

[ -n "$goto" ] && echo "uri $goto" > "$UZBL_FIFO"
#[ -n "$goto" ] && echo "uri $goto" | socat - "unix-connect:$UZBL_SOCKET"
