#!/usr/bin/env bash

_depcheck ()
{
    command -v "$1" > /dev/null || { echo "Missing dependency: $1."; return 1; }
}

# Check if no arguments are passed
if [ $# -eq 0 ]; then
    echo "Usage: $0 {-d: Dark|-l: Light}"
    exit 1
fi

XMONAD="$XDG_CONFIG_HOME/xmonad/lib/Colors"
XMOBAR="$XDG_CONFIG_HOME/xmobar/"
XRESOURCES="$XDG_CONFIG_HOME/X11"
ROFI="$HOME/.config/rofi/"
# Use a case statement to handle different arguments
case $1 in
    -d)
        echo "Dark Mode"
        # Add logic to start the service
        sed -i '3,$d' $XMONAD/Current.hs
        cat $XMONAD/Dark.hs >> $XMONAD/Current.hs
        cat $XMOBAR/src/xmobarrc_dark > $XMOBAR/xmobarrc
        cat $XRESOURCES/colors/citylights > $XRESOURCES/active_theme
        cat $ROFI/colors/modus-vivendi.rasi > $ROFI/colors.rasi
        emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme 'modus-vivendi-tinted))"  > /dev/null
        ;;
    -l)
        echo "Light Mode"
        # Add logic to stop the service
        sed -i '3,$d' $XMONAD/Current.hs
        cat $XMONAD/Light.hs >> $XMONAD/Current.hs
        cat $XMOBAR/src/xmobarrc_light > $XMOBAR/xmobarrc
        cat $XRESOURCES/colors/operandi > $XRESOURCES/active_theme
        cat $ROFI/colors/modus-operandi.rasi > $ROFI/colors.rasi
        # redshift -O 4500
        emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme 'modus-operandi))" > /dev/null
        # picom-toggle &
        ;;
    *)
        echo "Invalid option. Usage: $0 {-d: Dark|-l: Light}"
        exit 1
        ;;
esac

# _rofi () {
#     _depcheck rofi
#     active_theme="$HOME"/.config/rofi/colors.rasi
#     [ -f "$active_theme" ] || { echo "No rofi theme. Did you 'stow' your configurations?"; return 1; }
#     case $wm_theme
#     in
#         light) echo -e '@import "colors/modus-operandi.rasi"' > "$active_theme" ;;
#         dark) echo -e '@import "colors/modus-vivendi.rasi"' > "$active_theme" ;;
#     esac
# }

# _emacs ()
# {
#     _depcheck emacs

#     pgrep -x emacs > /dev/null || return 1

#     local light_theme
#     local dark_theme

#     light_theme="modus-operandi"
#     dark_theme="modus-vivendi"

#     # Just switch to the appropriate theme for Emacs:
#     # https://github.com/protesilaos/modus-themes
#     case "$1"
#     in
#         # TODO 2023-03-01: Define a function to load appropriate Emacs
#         # theme.
#         l*) emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme '${light_theme}))" > /dev/null ;;
#         d*) emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme '${dark_theme}))"  > /dev/null ;;
#     esac
# }

# # _xmonad () {
# #     wm_theme_file="$HOME"/.config/ukiran_active_theme

# #     echo "$style" > "$wm_theme_file"
# #     wm_theme="$style"
# #     _emacs "$style" &
# #     _rofi &
# # }

xrdb ~/.config/X11/xresources &
echo -e 'Restarting Xmonad...'
xmonad-restart.sh &
