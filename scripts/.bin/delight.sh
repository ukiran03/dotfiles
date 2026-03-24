#!/usr/bin/env bash

CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}"
STATE_FILE="${XDG_DATA_HOME:-$HOME/.local/share}/global_theme"
WALLPAPER="${XDG_DATA_HOME:-$HOME/.local/share}/bg"
gtk_theme_light="Adwaita"
gtk_theme_dark="Nordic-darker"
gtk_theme_dark_fallback="Adwaita-dark"

icon_theme_light="Papirus-Light"
icon_theme_dark="Papirus-Dark"

cursor_theme_light="Bibata-Modern-Classic"
cursor_theme_dark="Bibata-Modern-Ice"

_depcheck() {
	command -v "$1" >/dev/null || {
		echo "Missing dependency: $1."
		return 1
	}
}

_sed() {
	# NOTE 2021-08-27: Confirm the following.
	#
	# If we run this asynchronously other processes might fail to get
	# the new colours.  To my knowledge as of 2019-06-27, this appears
	# to be better than using sleep and/or until...
	sed --follow-symlinks -i "$@"
}

_emacs() {
	_depcheck emacs
	pgrep -x emacs >/dev/null || return 1
	case "$1" in
	light) emacsclient -e "(when (and (functionp 'modus-themes-get-themes) (modus-themes-get-themes)) (modus-themes-load-theme 'modus-operandi))" >/dev/null ;;
	dark) emacsclient -e "(when (and (functionp 'modus-themes-get-themes) (modus-themes-get-themes)) (modus-themes-load-theme 'modus-vivendi-tinted))" >/dev/null ;;
	esac
}

_rofi() {
	_depcheck rofi
	local rofi_dir="$CONFIG_DIR/rofi"
	local theme_dir="$rofi_dir/themes"
	local theme_file="$rofi_dir/colors.rasi"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Rofi: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) printf '@import "themes/modus-operandi.rasi"\n' >"$theme_file" ;;
	dark) printf '@import "themes/modus-vivendi.rasi"\n' >"$theme_file" ;;
	*) echo "Usage: _rofi {light|dark}" && return 1 ;;
	esac
	echo "[rofi]: Switched to $1 theme."
}

_xsettingsd() {
	_depcheck xsettingsd
	local xset_dir="$CONFIG_DIR/xsettingsd"
	local theme_dir="$xset_dir/themes"
	local theme_file="$xset_dir/xsettingsd.conf"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Xsettingsd: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) ln -sf "$theme_dir/xsettingsd-light.conf" "$theme_file" ;;
	dark) ln -sf "$theme_dir/xsettingsd-dark.conf" "$theme_file" ;;
	*) echo "Usage: _xsettingsd {light|dark}" && return 1 ;;
	esac

	# Kill and run again to read the new values.
	pgrep -xo xsettingsd >/dev/null && pkill -xo xsettingsd
	xsettingsd -c "$theme_file" &
	echo "[xsettingsd]: Switched to $1 theme."
}

_xmonad() {
	local xmonad_dir="$CONFIG_DIR/xmonad"
	local theme_dir="$xmonad_dir/lib/Colors"
	local theme_file="$theme_dir/Current.hs"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Xmonad: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) ln -sf "$theme_dir/Light.hs" "$theme_file" ;;
	dark) ln -sf "$theme_dir/Dark.hs" "$theme_file" ;;
	*) echo "Usage: _xmonad {light|dark}" && return 1 ;;
	esac
}

_restart_xmonad() {
	echo "Recompiling XMonad..."
	if xmonad --recompile; then
		# Ensure the restart script exists/is executable before backgrounding
		if command -v xmonad-restart.sh >/dev/null; then
			xmonad-restart.sh &
		fi
		echo "[xmonad]: Switched to $1 theme."
	else
		echo "Error: XMonad compilation failed. Check your Haskell syntax."
		return 1
	fi
}

_xmobar() {
	_depcheck xmobar
	local xmobar_dir="$CONFIG_DIR/xmobar"
	local theme_dir="$xmobar_dir/src"
	local theme_file="$xmobar_dir/xmobarrc"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Xmobar: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) ln -sf "$theme_dir/xmobarrc_light" "$theme_file" ;;
	dark) ln -sf "$theme_dir/xmobarrc_dark" "$theme_file" ;;
	*) echo "Usage: _xmobar {light|dark}" && return 1 ;;
	esac
	echo "[xmobar]: Switched to $1 theme."
}

_system_theme() {
	local sys_theme_file="${XDG_DATA_HOME:-$HOME/.local/share}/global_theme_env"
	case "$1" in
	light | dark)
		printf 'export SYSTEM_THEME="%s"\n' "$1" >"$sys_theme_file"
		if [ -f "$HOME/.profile" ]; then
			source "$HOME/.profile"
		fi
		echo "System theme set to: $1"
		;;
	*)
		echo "Usage: _system_theme {light|dark}"
		return 1
		;;
	esac
}

#### Gsettings

# Get or set the GTK theme
_gsettings_gtk_act_theme_interface() {
	gsettings "$1" org.gnome.desktop.interface gtk-theme "${@:2}"
}

# Set the Color Scheme
_gsettings_color_scheme_set() {
	gsettings set org.gnome.desktop.interface color-scheme "$1"
}

# Browsers uses this
_gsettings_theme() {
	_depcheck gsettings
	if [ "$(_gsettings_gtk_act_theme_interface get)" = "'$gtk_theme_light'" ]; then
		_gsettings_gtk_act_theme_interface set "$gtk_theme_dark"
	else
		_gsettings_gtk_act_theme_interface set "$gtk_theme_light"
	fi

	if [ "$(gsettings get org.gnome.desktop.interface color-scheme)" = "'prefer-light'" ]; then
		style=dark
		_gsettings_color_scheme_set prefer-dark
	else
		style=light
		_gsettings_color_scheme_set prefer-light
	fi
}

_xresources() {
	local x11_dir="$CONFIG_DIR/X11"
	local theme_dir="$x11_dir/themes"
	local theme_file="$x11_dir/active_theme"
	local src_file="$x11_dir/xresources"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Xresources: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light)
		ln -sf "$theme_dir/light_xresources" "$theme_file"
		printf "export XCURSOR_THEME=\"$cursor_theme_light\"\n" >"$x11_dir/xcursor_env"
		;;
	dark)
		ln -sf "$theme_dir/dark_xresources" "$theme_file"
		printf "export XCURSOR_THEME=\"$cursor_theme_dark\"\n" >"$x11_dir/xcursor_env"
		;;
	*) echo "Usage: _xresources {light|dark}" && return 1 ;;
	esac

	if xrdb -I "$HOME" -load "$src_file" 2>/dev/null; then
		source "${HOME}/.profile"
		echo "[xresources]: Switched to $1 theme."
	else
		echo "Error: Failed to load Xresources via xrdb."
		return 1
	fi
}

_starship() {
    _depcheck starship || return 1

    local starship_config_dir="$CONFIG_DIR/starship"
    local theme_dir="$starship_config_dir/themes"
    local theme_file="$starship_config_dir/normal.toml"

    if [[ ! -d "$theme_dir" ]]; then
        echo "Error: Starship: $theme_dir not found."
        return 1
    fi

    case "$1" in
        light)
            ln -sf "$theme_dir/light.toml" "$theme_file"
            ;;
        dark)
            ln -sf "$theme_dir/dark.toml" "$theme_file"
            ;;
        *)
            echo "Usage: _starship {light|dark}"
            return 1
            ;;
    esac
    echo "[starship]: Switched to $1 theme."
}

_zathura() {
	_depcheck zathura
	local zathura_dir="$CONFIG_DIR/zathura"
	local theme_dir="$zathura_dir/src"
	local theme_file="$zathura_dir/zathurarc"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Zathura: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) ln -sf "$theme_dir/zathurarc-light" "$theme_file" ;;
	dark) ln -sf "$theme_dir/zathurarc-dark" "$theme_file" ;;
	*) echo "Usage: _zathura {light|dark}" && return 1 ;;
	esac
	echo "[zathura]: Switched to $1 theme."
}

_dunst() {
	_depcheck dunst
	local dunst_dir="$CONFIG_DIR/dunst"
	local theme_dir="$dunst_dir/src"
	local theme_file="$dunst_dir/dunstrc"
	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Dunst: $theme_dir not found."
		return 1
	fi
	case "$1" in
	light) ln -sf "$theme_dir/dunstrc_light" "$theme_file" ;;
	dark) ln -sf "$theme_dir/dunstrc_dark" "$theme_file" ;;
	*) echo "Usage: _dunst {light|dark}" && return 1 ;;
	esac
	echo "[dunst]: Switched to $1 theme."
}

_wallpaper() {
	case "$1" in
	light) setbg -s $(find ~/Pictures/walls/light/ -type f | shuf -n 1) & ;;
	dark) setbg -s $(find ~/Pictures/walls/dark/ -type f | shuf -n 1) & ;;
	*) echo "Usage: _wallpaper {light|dark}" && return 1 ;;
	esac
	echo "[wallpaper]: Switched to $1 theme."
}

_global_env() {
	local theme
	if [[ -f "$STATE_FILE" ]]; then
		theme=$(<"$STATE_FILE")
		if [[ "$1" == "$theme" ]]; then
			return 0 # Success
		fi
	fi
	return 1 # Failure (file missing or theme mismatch)
}

apps=(_xmonad _emacs _rofi _xmobar _xresources _starship _zathura _dunst)

# 1. Map flags to mode names first
case "$1" in
-l | light) mode="light" ;;
-d | dark) mode="dark" ;;
*)
	echo "Usage: $0 {-d|-l} [-f: Force]"
	exit 1
	;;
esac

# The Logic: Check if we should skip the "Already Set" check
# We run the theme change IF:
#   A) The force flag (-f) is passed as the second argument
#   B) OR we are NOT already in that environment (theme)
if [[ "$2" == "-f" ]] || ! _global_env "$mode"; then
	echo "Applying $mode theme..."
	[[ "$2" == "-f" ]] && echo "(Forced to override active)"
	# Update the state file
	echo "$mode" >"$STATE_FILE"

	for app in "${apps[@]}"; do
		$app "$mode" & # as a background job
	done

	# This pauses the script until ALL background jobs started above
	# are done
	wait

	## Post Actions
	killall dunst || true # to reload dunst, and ignore exit status.
	_wallpaper "$mode"
	_system_theme "$mode"
	_xsettingsd "$mode"
    _gsettings_theme
	_restart_xmonad # restart xmonad
	# betterlockscreen -u "$WALLPAPER" > /dev/null 2>&1 &
else
	echo "Already using $mode theme. Use -f to force."
	exit 0
fi
