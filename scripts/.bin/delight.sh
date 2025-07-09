#!/usr/bin/env bash

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
        # sed -i '3,$d' $XMONAD/Current.hs
        cat $XMONAD/Dark.hs > $XMONAD/Current.hs
        cat $XMOBAR/src/xmobarrc_dark > $XMOBAR/xmobarrc
        cat $XRESOURCES/colors/citylights > $XRESOURCES/active_theme
        cat $ROFI/colors/modus-vivendi.rasi > $ROFI/colors.rasi
        emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme 'modus-vivendi-tinted))"  > /dev/null
        ;;
    -l)
        echo "Light Mode"
        # Add logic to stop the service
        # sed -i '3,$d' $XMONAD/Current.hs
        cat $XMONAD/Light.hs > $XMONAD/Current.hs
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

xrdb ~/.config/X11/xresources &
echo -e 'Restarting Xmonad...'
xmonad-restart.sh &
