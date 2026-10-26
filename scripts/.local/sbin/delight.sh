#!/usr/bin/env bash

CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}"
STATE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}"
STATE_FILE="$STATE_DIR/global_theme"
WALLPAPER_DIR_LIGHT="$HOME/Pictures/walls/light"
WALLPAPER_DIR_DARK="$HOME/Pictures/walls/dark"

gtk_theme_light="Adwaita"
gtk_theme_dark="Nordic-darker"
gtk_theme_dark_fallback="Adwaita-dark"
icon_theme_light="Papirus-Light"
icon_theme_dark="Papirus-Dark"
cursor_theme_light="Bibata-Modern-Classic"
cursor_theme_dark="Bibata-Modern-Ice"

# Ensure necessary data directory exists before writing state files
mkdir -p "$STATE_DIR"

_depcheck() {
	command -v "$1" >/dev/null || {
		echo "Missing dependency: $1."
		return 1
	}
}

_current_wm() {
	wmctrl -m 2>/dev/null | awk '/^Name:/ {print $2}'
}

_sed() {
	sed --follow-symlinks -i "$@"
}

_redshift() {
	_depcheck rso
	case "$1" in
	light) rso 3500 ;;
	dark) rso 4500 ;;
	esac
	return 0
}

_btop() {
	# Replace 'my_new_theme' with your target theme name
	NEW_THEME="my_new_theme"
	BTOP_CONF="$HOME/.config/btop/btop.conf"

	if [ -f "$BTOP_CONF" ]; then
		sed -i "s/^color_theme = .*/color_theme = \"$NEW_THEME\"/" "$BTOP_CONF"
	fi
}

_emacs() {
	local custom_file="$HOME/.emacs.d/custom.el"
	local light_theme="modus-operandi"
	local dark_theme="modus-vivendi-tinted"
	local new_theme

	case "$1" in
	light) new_theme="$light_theme" ;;
	dark) new_theme="$dark_theme" ;;
	*)
		echo "Usage: _emacs [light|dark]"
		return 1
		;;
	esac

	if pgrep -x emacs >/dev/null; then
		emacsclient -e "(when (and (fboundp 'modus-themes-get-themes) (fboundp 'consult-theme)) (consult-theme '$new_theme))" >/dev/null 2>&1
	else
		touch "$custom_file"
		awk -v found=0 -v theme="$new_theme" '
			/\(setq my-theme \x27/ {
				found=1;
				$0="(setq my-theme \x27" theme ")"
			}
			{ print }
			END {
				if (!found) print "(setq my-theme \x27" theme ")"
			}
		' "$custom_file" >"${custom_file}.tmp" && mv "${custom_file}.tmp" "$custom_file"
	fi
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
	local mode="$1"
	echo "Recompiling XMonad..."
	if xmonad --recompile; then
		if command -v xmonad-restart.sh >/dev/null; then
			xmonad-restart.sh &
		fi
		echo "[xmonad]: Switched to $mode theme."
	else
		echo "Error: XMonad compilation failed. Check your Haskell syntax."
		return 1
	fi
}

_herbstluftwm() {
	echo "TODO"
}

_restart_herbstluftwm() {
	local mode="$1"
	echo "Reloading Herbstluftwm..."
	if herbstclient reload; then
		echo "[herbstluftwm]: Switched to $mode theme."
	else
		echo "Error: Herbstluftwm reloading failed."
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

_polybar() {
	_depcheck polybar
	local polybar_dir="$CONFIG_DIR/polybar"
	local theme_dir="$polybar_dir/themes"
	local theme_file="$polybar_dir/colors.ini"

	if [[ ! -d "$theme_dir" ]]; then
		echo "Error: Polybar: $theme_dir not found."
		return 1
	fi

	case "$1" in
	light) ln -sf "$theme_dir/polybar_light.ini" "$theme_file" ;;
	dark) ln -sf "$theme_dir/polybar_dark.ini" "$theme_file" ;;
	*) echo "Usage: _polybar {light|dark}" && return 1 ;;
	esac
	echo "[polybar]: Switched to $1 theme."
}

_system_theme() {
	local sys_theme_file="$STATE_DIR/global_theme_env"
	case "$1" in
	light | dark)
		printf 'export SYSTEM_THEME="%s"\n' "$1" >"$sys_theme_file"
		if [[ -f "$HOME/.profile" ]]; then
			. "$HOME/.profile"
		fi
		echo "System theme set to: $1"
		;;
	*)
		echo "Usage: _system_theme {light|dark}"
		return 1
		;;
	esac
}

_gsettings_theme() {
	_depcheck gsettings
	case "$1" in
	light)
		gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme_light"
		gsettings set org.gnome.desktop.interface color-scheme "prefer-light"
		echo "[gsettings]: Switched to light theme."
		;;
	dark)
		gsettings set org.gnome.desktop.interface gtk-theme "$gtk_theme_dark"
		gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
		echo "[gsettings]: Switched to dark theme."
		;;
	*)
		echo "Usage: _gsettings_theme {light|dark}"
		return 1
		;;
	esac
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
		printf 'export XCURSOR_THEME="%s"\n' "$cursor_theme_light" >"$x11_dir/xcursor_env"
		;;
	dark)
		ln -sf "$theme_dir/dark_xresources" "$theme_file"
		printf 'export XCURSOR_THEME="%s"\n' "$cursor_theme_dark" >"$x11_dir/xcursor_env"
		;;
	*)
		echo "Usage: _xresources {light|dark}"
		return 1
		;;
	esac

	if xrdb -I "$HOME" -load "$src_file" 2>/dev/null; then
		[[ -f "$HOME/.profile" ]] && . "$HOME/.profile"
		echo "[xresources]: Switched to $1 theme."
	else
		echo "Error: Failed to load Xresources via xrdb."
		return 1
	fi
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
	local wall=""
	case "$1" in
	light)
		wall="$(find "$WALLPAPER_DIR_LIGHT" -type f 2>/dev/null | shuf -n 1)"
		;;
	dark)
		wall="$(find "$WALLPAPER_DIR_DARK" -type f 2>/dev/null | shuf -n 1)"
		;;
	*)
		echo "Usage: _wallpaper {light|dark}"
		return 1
		;;
	esac

	if [[ -n "$wall" ]]; then
		setbg -s "$wall" &
		echo "[wallpaper]: Switched to $1 theme."
	else
		echo "Error: No wallpapers found for $1."
	fi
}

_trayer() {
	_depcheck trayer
	killall trayer 2>/dev/null || true
	local light_color="0xf2f2f2"
	local dark_color="0x0d0e1c"
	local colorTrayer

	case "$1" in
	light) colorTrayer="$light_color" ;;
	dark | *) colorTrayer="$dark_color" ;;
	esac

	trayer --edge top --align right --widthtype request --SetDockType true \
		--SetPartialStrut true --expand true --transparent true \
		--alpha 0 --tint "$colorTrayer" --height 21 &
}

declare -g global_theme="dark" # fallback

if [[ -f "$STATE_FILE" ]]; then
	global_theme="$(<"$STATE_FILE")"
fi

case "$1" in
-l | light) mode="light" ;;
-d | dark) mode="dark" ;;
-t | toggle) [[ "$global_theme" == "dark" ]] && mode="light" || mode="dark" ;;
*)
	echo "Usage: $0 {-l|light|-d|dark|-t|toggle} [-f]"
	exit 1
	;;
esac

current_wm=$(_current_wm)
echo "Active window manager: $current_wm"

apps=(_xmonad _emacs _rofi _xresources _zathura _dunst _system_theme _xsettingsd _gsettings_theme _redshift)

if [[ "$2" == "-f" ]] || [[ "$global_theme" != "$mode" ]]; then
	echo "Applying $mode theme..."
	[[ "$2" == "-f" ]] && echo "(Forced to override active)"

	echo "$mode" >"$STATE_FILE"

	for app in "${apps[@]}"; do
		"$app" "$mode" &
	done

	killall dunst 2>/dev/null || true
	_wallpaper "$mode"

	if [[ "$current_wm" == "herbstluftwm" ]]; then
		echo "Restart Herbstluftwm"
		_restart_herbstluftwm "$mode"
	elif [[ "$current_wm" == "xmonad" ]]; then
		echo "Restart Xmonad"
		_restart_xmonad "$mode"
		_trayer "$mode"
	fi

	wait
else
	echo "Already using $mode theme. Use -f to force."
	exit 0
fi
