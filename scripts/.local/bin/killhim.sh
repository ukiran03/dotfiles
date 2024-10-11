#!/bin/sh
pid=$(ps -U $USER -o pid,%mem,%cpu,comm | sort -b -k2 -r | sed -n '1!p' | rofi -dmenu -i -l 8 | awk '{print $1}')
kill -15 $pid 2>/dev/null
# notify-send "  U killed me!"
dunstify -a "ignore" -i computer-fail "U killed me!" -r 3242

