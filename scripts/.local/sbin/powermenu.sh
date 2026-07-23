#!/usr/bin/env bash

# Define the display names
shutdown="Shutdown  (y/n)"
reboot="Reboot    (y/n)"
lock="Lock"
suspend="Suspend"
logout="Log Out"

# Pass options to rofi with their respective system icon names
options="$lock\0icon\x1fsystem-lock-screen\n$suspend\0icon\x1fsystem-suspend\n$logout\0icon\x1fsystem-log-out\n$reboot\0icon\x1fsystem-reboot\n$shutdown\0icon\x1fsystem-shutdown"

# Helper function for the confirmation dialog

confirm_exit() {
    echo -e "No\nYes" | rofi -dmenu -i -p "$1" \
    -theme-str 'window {width: 18em;} listview {lines: 2;} entry {placeholder: "Are you sure?";}' 2>/dev/null
}

# Get choice from main menu
chosen="$(echo -e "$options" | rofi -dmenu -i -p "Power Menu:" -show-icons -theme-str 'window {width: 18em;} listview {lines: 5;}' 2>/dev/null)"

if [ -z "$chosen" ]; then
    exit 0
fi

case $chosen in
    $shutdown)
        # Call the confirmation dialog
        confirmation=$(confirm_exit "Shutdown")
        if [ "$confirmation" = "Yes" ]; then
            loginctl poweroff
        fi
        ;;
    $reboot)
        # Optional: You can add it to reboot too!
        confirmation=$(confirm_exit "Reboot")
        if [ "$confirmation" = "Yes" ]; then
            loginctl reboot
        fi
        ;;
    $lock)
        betterlockscreen -l
        ;;
    $suspend)
        loginctl suspend
        # confirmation=$(confirm_exit "Suspend")
        # if [ "$confirmation" = "Yes" ]; then
        # fi
        ;;
    $logout)
        loginctl terminate-user $USER
        # confirmation=$(confirm_exit "Log Out")
        # if [ "$confirmation" = "Yes" ]; then
        # fi
        ;;
esac
