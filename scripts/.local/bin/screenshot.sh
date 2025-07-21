#!/usr/bin/env bash

Nimgpath="$HOME/Pictures/screenshots/$(date +%Y-%b-%d-%H%M%S).jpg"
Cimgpath="$HOME/Pictures/screenshots/$(date +%Y-%b-%d-%H%M%S-x).jpg"
Wimgpath="$HOME/Pictures/screenshots/$(date +%Y-%b-%d-%H%M%S-w).jpg"
_screenshot() {
    if [[ "$1" == "-s" ]]; then
        maim -m 8 -o --select "$Cimgpath"
        dunstify -a "ignore" -t 1000 -r 341 -i "$Cimgpath" "Selected" "saved screenshot"
    elif [[ "$1" == "-c" ]]; then
        maim -m 8 -o --select | tee "$Cimgpath" | xclip -selection clipboard -t image/jpeg
        dunstify -a "ignore" -t 1000 -r 341 -i "$Cimgpath" "Clipboard" "saved screenshot"
    elif [[ "$1" == "-w" ]]; then
        maim -m 8 -i $(xdotool getactivewindow) "$Wimgpath"
        dunstify -a "ignore" -t 1000 -r 341 -i "$Wimgpath" "Window" "saved screenshot"
    else
        maim -m 8 --hidecursor "$Nimgpath"
        dunstify -a "ignore" -t 1000 -r 341 -i "$Nimgpath" "Full" "saved screenshot"
    fi
}

# Call the function with the first argument
_screenshot "$1"
