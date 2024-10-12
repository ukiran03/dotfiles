export QT_QPA_PLATFORMTHEME=qt5ct

export ERRFILE="$XDG_CONFIG_HOME/X11/xsession-errors"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
source "$HOME"/.zshenv

xrdb -load "$XDG_CONFIG_HOME/X11/xresources"

# Source my bashrc.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME"/.bashrc ]; then
        source "$HOME"/.bashrc
    fi
fi

# Set PATH so it includes user's private executables.
if [ -d "$HOME"/.bin ]; then
    PATH=$PATH:"$HOME"/.bin
fi

# This is another possible location for user-specific binaries.
if [ -d "$HOME"/.local/bin ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi
