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

export ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"

export LD_LIBRARY_PATH=/usr/local/lib/
export XAUTHORITY=$HOME/.Xauthority
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Dotfiles
export DOTFILES="$HOME/dotfiles"

# GOPATH :Go
export GOPATH="$XDG_DATA_HOME"/go
export GOBIN="$GOPATH"/bin
export PATH=$PATH:$GOPATH/bin

xrdb -load "$XDG_CONFIG_HOME/X11/xresources"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"
export XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"
export PRETTIERD_DEFAULT_CONFIG="~/.config/prettier/.prettierrc"
export ANDROID_USER_HOME="$XDG_DATA_HOME"/android
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export GHCUP_USE_XDG_DIRS=true
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority
export NODE_REPL_HISTORY="$XDG_STATE_HOME"/node_repl_history
export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"
export PYTHON_HISTORY="$XDG_STATE_HOME/python_history"
export SQLITE_HISTORY="$XDG_STATE_HOME"/sqlite_history

# export STACK_ROOT="$XDG_DATA_HOME"/stack
# export STACK_XDG=1

export PROFILE_CHECK="!!.profile has been sourced!!"

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

# pnpm
export PNPM_HOME="/home/ukiran/.local/share/pnpm"
if [[ -d "$PNPM_HOME" && ! ":$PATH:" == *":$PNPM_HOME:"* ]]; then
  export PATH="$PNPM_HOME:$PATH"
fi
# pnpm end
