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

# Svdir
export SVDIR=$HOME/.config/service

# Dotfiles
export DOTFILES="$HOME/dotfiles"

# GOPATH :Go
export GOPATH="$XDG_DATA_HOME"/go
export GOBIN="$GOPATH"/bin
export PATH=$PATH:$GOPATH/bin

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
export HISTFILE="${XDG_STATE_HOME}"/bash/history
export CALCHISTFILE="$XDG_CACHE_HOME"/calc_history

export ELM_HOME="$XDG_CONFIG_HOME"/elm

export ANDROID_USER_HOME="$XDG_DATA_HOME"/android
alias adb='HOME="$XDG_DATA_HOME"/android adb'

eval $(dircolors "$XDG_CONFIG_HOME"/dircolors)

export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo

# [stack]: /home/ukiran/.stack
export STACK_ROOT="$XDG_DATA_HOME"/stack
export STACK_XDG=1

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

## cargo
export PATH="$HOME/.local/share/cargo/bin:$PATH"

# parallel
export PARALLEL_HOME="$XDG_CONFIG_HOME"/parallel

# ruby bundler: /home/ukiran/.bundle
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle

# Odin
export ODIN_ROOT="/usr/local/share/odin"

# npm
export NPM_CONFIG_INIT_MODULE="$XDG_CONFIG_HOME"/npm/config/npm-init.js
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME"/npm
export NPM_CONFIG_TMP="$XDG_RUNTIME_DIR"/npm
# pnpm
export PNPM_HOME="/home/ukiran/.local/share/pnpm"
if [[ -d "$PNPM_HOME" && ! ":$PATH:" == *":$PNPM_HOME:"* ]]; then
  export PATH="$PNPM_HOME:$PATH"
fi
# pnpm end
#. "/home/ukiran/.deno/env"
# eval "$(~/.local/bin/mise activate)"

#[ -f "/home/ukiran/.ghcup/env" ] && . "/home/ukiran/.ghcup/env" # ghcup-env

#eval "$(mise activate zsh)"

## mpd for xmobar
export MPD_HOST="127.0.0.1"
export MPD_PORT="6600"

# . "$HOME/.atuin/bin/env"

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus

export XCURSOR_SIZE=18
# export XCURSOR_THEME="Bibata-Modern-Classic"

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/X11/xcursor_env" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/X11/xcursor_env"

[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/global_theme_env" ] && . "${XDG_DATA_HOME:-$HOME/.local/share}/global_theme_env"

# This ensures that any script you run manually OR via turnstile
# (if turnstile is configured to import env) sees the bus.
## Remoce check
# export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"
# export XDG_RUNTIME_DIR="/run/user/$(id -u)"
