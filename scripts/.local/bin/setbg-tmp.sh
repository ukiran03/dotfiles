#!/bin/sh

# This script does the following:
#	Run by itself, set the wallpaper (at X start).
#	If given a file, set that as the new wallpaper.
#	If given a directory, choose random file in it.
#	If wal is installed, also generates a colorscheme.

# Location of link to wallpaper link.
bgloc="${XDG_DATA_HOME:-$HOME/.local/share}/bg"

# Give -s as parameter to make notifications silent.
while getopts "s" o; do case "${o}" in
	s) silent='1' ;;
	esac done

shift $((OPTIND - 1))

trueloc="$(readlink -f "$1")" &&
	case "$(file --mime-type -b "$trueloc")" in
	image/*) ln -sf "$trueloc" "$bgloc" && [ -z "$silent" ] && dunstify -t 1500 -r 3455 -a "ignore" -i "$bgloc" "Changing wallpaper..." ;;
	inode/directory) ln -sf "$(find "$trueloc" -iregex '.*.\(jpg\|jpeg\|png\|gif\)' -type f | shuf -n 1)" "$bgloc" && [ -z "$silent" ] && dunstify -t 1500 -r 3455 -a "ignore" -i "$bgloc" "Random Wallpaper chosen." ;;
	*)
		[ -z "$silent" ] && dunstify -t 1500 -r 3455 -a "ignore" "🖼️ Error" "Not a valid image or directory."
		exit 1
		;;
	esac

# xwallpaper --zoom "$bgloc"
feh --bg-scale "$bgloc"
