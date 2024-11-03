#!/usr/bin/env bash

# Location of link to wallpaper link.
bgloc="${XDG_DATA_HOME:-$HOME/.local/share}/bg"

_notify() {
    case $state in
        1)
}

# Give -s as parameter to make notifications silent.
while getopts "s" o; do case "${o}" in
	s) silent='1' ;;
esac done

shift $((OPTIND - 1))

trueloc="$(readlink -f "$1")" &&
ln -sf "$trueloc" "$bgloc" && [ -z "$silent" ] && dunstify -t 1500 -r 3455 -a "ignore" -i "$bgloc" "Changing wallpaper..."

# xwallpaper --zoom "$bgloc"
feh --bg-scale "$bgloc"
