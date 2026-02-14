#!/usr/bin/env bash

up_icon="uk-brightness-high"
down_icon="uk-brightness-low"
reset_icon="uk-brightness-medium"
DEFAULT_BRIGHTNESS="45%"
PIPE_PATH="$HOME/.config/xmobar/fifo/brightness-fifo"


# Ensure the pipe exists, create otherwise
mkdir -p "$(dirname "$PIPE_PATH")"
if [[ ! -p "$PIPE_PATH" ]]; then
    [[ -f "$PIPE_PATH" ]] && rm -f "$PIPE_PATH"
    mkfifo "$PIPE_PATH"
fi


_check() {
	if ! command -v "$1" >/dev/null 2>&1; then
		echo "Error: $1 is not installed." >&2
		exit 1
	fi
}

_check "brightnessctl"
_check "dunstify"

get_brightness() {
	brightnessctl -m | awk -F, '{print substr($4, 1, length($4)-1)}'
}

write_to_pipe() {
    # Attempt to write to the pipe.  Redirecting to a background
    # subshell prevents the main script from hanging if xmobar isn't
    # reading.
    ( printf "%s\n" "$1" > "$PIPE_PATH" ) &

    # Cleanly detach the background job
    disown %1 2>/dev/null
}

send_notification() {
	local val=$1
	local icon=$2
	local msg=$3
	dunstify -a "system" -u low -r 4434 -h int:value:"$val" \
		-i "$icon" "Brightness: ${val}%" "${msg}" -t 2000
}

case "$1" in
up)
	brightnessctl set +5% >/dev/null
	send_notification "$(get_brightness)" "$up_icon" "Increasing"
	;;
down)
	brightnessctl set 5%- >/dev/null
	send_notification "$(get_brightness)" "$down_icon" "Decreasing"
	;;
reset)
	brightnessctl set "$DEFAULT_BRIGHTNESS" >/dev/null
	send_notification "${DEFAULT_BRIGHTNESS//%/}" "$reset_icon" "Restored to Default"
	;;
*)
	echo "Usage: $0 {up|down|reset}"
    echo "$(get_brightness)%"
	# exit 1
	;;
esac

write_to_pipe "$(get_brightness)%"
