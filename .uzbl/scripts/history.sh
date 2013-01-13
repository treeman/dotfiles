#!/bin/sh

UZBL_HISTORY_FILE=$HOME/.uzbl/data/history

[ -n "$UZBL_PRIVATE" ] && exit 0

>> "$UZBL_HISTORY_FILE" || exit 1

echo "$( date +'%Y-%m-%d %H:%M:%S' ) $UZBL_URI $UZBL_TITLE" >> "$UZBL_HISTORY_FILE"

