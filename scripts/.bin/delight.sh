#!/usr/bin/env bash

# Check if no arguments are passed
if [ $# -eq 0 ]; then
    echo "Usage: $0 {-d: Dark|-l: Light}"
    exit 1
fi

XMONAD="$XDG_CONFIG_HOME/xmonad/"
XMOBAR="$XDG_CONFIG_HOME/xmobar/"
XRESOURCES="$XDG_CONFIG_HOME/X11"
ROFI="$HOME/.config/rofi/"
ZATHURA="$HOME/.config/zathura/"
# Use a case statement to handle different arguments
case $1 in
    -d)
        echo "Dark Mode"
        # Add logic to start the service
        # sed -i '3,$d' $XMONAD/Current.hs
        cat $XMONAD/lib/Colors/Dark.hs > $XMONAD/lib/Colors/Current.hs
        cat $XMOBAR/src/xmobarrc_dark > $XMOBAR/xmobarrc
        cat $XRESOURCES/colors/citylights > $XRESOURCES/active_theme
        cat $ROFI/colors/modus-vivendi.rasi > $ROFI/colors.rasi
        cat $ZATHURA/src/zathurarc-dark > $ZATHURA/zathurarc
        emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme 'modus-vivendi-tinted))"  > /dev/null
        # Xfce
        xfconf-query -c xsettings -p /Net/ThemeName -s "Nordic-darker"
        xfconf-query -c xsettings -p /Net/IconThemeName -s "Papirus-Dark"
        # xfconf-query -c xsettings -p /Gtk/CursorThemeName -s "Bibata-Modern-Ice"
        ## wallpaper
        setbg -s $(find ~/Pictures/walls/dark/ -type f | shuf -n 1) &
        ;;
    -l)
        echo "Light Mode"
        # Add logic to stop the service
        # sed -i '3,$d' $XMONAD/Current.hs
        # cat $XMONAD/Light.hs > $XMONAD/Current.hs
        cat $XMONAD/lib/Colors/Light.hs > $XMONAD/lib/Colors/Current.hs
        cat $XMOBAR/src/xmobarrc_light > $XMOBAR/xmobarrc
        cat $XRESOURCES/colors/operandi > $XRESOURCES/active_theme
        cat $ROFI/colors/modus-operandi.rasi > $ROFI/colors.rasi
        cat $ZATHURA/src/zathurarc-light > $ZATHURA/zathurarc
        # redshift -O 4500
        emacsclient -e "(when (and (functionp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-load-theme 'modus-operandi))" > /dev/null
        # picom-toggle &
        # Xfce
        xfconf-query -c xsettings -p /Net/ThemeName -s "Adwaita"
        xfconf-query -c xsettings -p /Net/IconThemeName -s "Papirus-Light"
        # xfconf-query -c xsettings -p /Gtk/CursorThemeName -s "Bibata-Modern-Classic"
        ## wallpaper
        setbg -s $(find ~/Pictures/walls/light/ -type f | shuf -n 1) &
        ;;
    *)
        echo "Invalid option. Usage: $0 {-d: Dark|-l: Light}"
        exit 1
        ;;
esac

xrdb ~/.config/X11/xresources &
echo -e 'Restarting Xmonad...'
xmonad-restart.sh &
