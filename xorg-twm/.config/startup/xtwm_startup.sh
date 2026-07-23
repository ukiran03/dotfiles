#!/usr/bin/env bash

# ==============================================================================
# HELPERS
# ==============================================================================

# Verify if a command/executable exists in the user's $PATH
_check() {
	command -v "$1" >/dev/null 2>&1
}

# Launch a background application if it isn't already running
_launch() {
	# $1 = Process Name (for pgrep)
	# $2 = Full Command to execute
	local binary="${2%% *}"

	if _check "$binary"; then
		if pgrep -f "$1" >/dev/null; then
			echo "   [Active] '$1' is already running."
			return
		else
			echo "   [Launch] Starting '$1'..."
			eval "$2" &
		fi
	else
		echo "   [Error]  Command '$binary' not found in PATH."
	fi
}

# Launch a background Tmux session if it doesn't exist
_launch_tmux() {
	# $1 = Session Name
	if _check "tmux"; then
		if tmux has-session -t "$1" 2>/dev/null; then
			echo "   [Active] 'tmux' session '$1' is already running."
		else
			echo "   [Launch] Starting tmux session '$1'..."
			tmux new-session -d -s "$1"
		fi
	else
		echo "   [Error]  'tmux' is not installed."
	fi
}

# ==============================================================================
# STARTUP EXECUTION
# ==============================================================================

echo "Initializing environment..."

# --- Base X11 Environment & Theme Config ---
_check xsetroot && xsetroot -cursor_name left_ptr &
_check feh && $HOME/.fehbg &
xrdb -load "$XDG_CONFIG_HOME/X11/xresources" 2>/dev/null || true

# --- System Daemons & Utilities ---
_launch "dunst" "dunst"
_launch "xfce4-power-manager" "xfce4-power-manager"
_launch "xsettingsd" "xsettingsd -c ~/.config/xsettingsd/xsettingsd.conf"
_launch "greenclip" "greenclip daemon"
_launch "sxhkd" "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"

_check "numlockx" && numlockx on
_check "xmodmap" && xmodmap "$XDG_CONFIG_HOME/X11/xmodmap" &

# --- Core Desktop Services ---
_launch "thunar" "thunar --daemon"
_launch "urxvtd" "urxvtd -q"
_launch_tmux "Main"

# --- Contextual Tools & Overlays ---
if _check "redshift" && _check "rso"; then
	rso >/dev/null 2>&1
fi

# --- Misc ---
if _check "laptop-brightness.sh"; then
	laptop-brightness.sh >/dev/null 2>&1
fi
_launch "godoc" "$HOME/.bin/godocs.sh"

echo "Environment initialized successfully."
