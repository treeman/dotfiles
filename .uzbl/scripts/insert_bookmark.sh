#!/bin/sh

UZBL_BOOKMARKS_FILE=$HOME/.uzbl/data/bookmarks

>> "$UZBL_BOOKMARKS_FILE" || exit 1

which zenity >/dev/null 2>&1 || exit 2

tags="$( zenity --entry --text="Enter space-separated tags for bookmark $UZBL_URI:" )"
exitstatus="$?"
[ "$exitstatus" -eq 0 ] || exit "$exitstatus"

# TODO: check if already exists, if so, and tags are different: ask if you want to replace tags
echo "$UZBL_URI $tags" >> "$UZBL_BOOKMARKS_FILE"
echo "set uri_color = @uri_bm_color" > "$UZBL_FIFO"

