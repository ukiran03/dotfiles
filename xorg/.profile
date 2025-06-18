export TERM=xterm
export VOID_PACKAGES="$HOME/.local/share/void-packages/"
export QT_QPA_PLATFORMTHEME=qt5ct

export ERRFILE="$XDG_CONFIG_HOME/X11/xsession-errors"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"

# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export LD_LIBRARY_PATH=/usr/local/lib/
export XAUTHORITY=$HOME/.Xauthority
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# GOPATH :Go
export GOPATH="$XDG_DATA_HOME"/go
export GOBIN="$GOPATH"/bin
export PATH=$PATH:$GOPATH/bin

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

export PODMAN_IGNORE_CGROUPSV1_WARNING

#. "/home/ukiran/.local/share/cargo/env"

[ -f "/home/ukiran/.ghcup/env" ] && . "/home/ukiran/.ghcup/env" # ghcup-env
