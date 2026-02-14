#!/usr/bin/env bash

FILE="/sys/class/power_supply/BAT1/status"
ICON="battery-empty"

send_notify() {
	local msg="Battery running out!!"

	if command -v dunstify >/dev/null; then
		dunstify -i "$ICON" -u critical -r 2389 "$msg"
	else
		notify-send -i "$ICON" -u critical "$msg"
	fi
}

if [[ ! -f "$FILE" ]] || [[ "$(<"$FILE")" == "Discharging" ]]; then
	send_notify
fi

# Ensures the script reports "Success" regardless of the battery
# state
exit 0
