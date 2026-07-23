# ==============================================================================
# Environment, Localization & Terminal Settings
# ==============================================================================
export TERM="xterm"
export PROFILE_CHECK="!!.profile has been sourced!!"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Preferred Applications
export EDITOR="emacs -q --load ~/.mini-emacs.el -nw"
export VISUAL="emacs -q --load ~/.mini-emacs.el -nw"
export TERMINAL="alacritty"
export BROWSER="firefox"

# Pager & Man pages
export GROFF_NO_SGR=1
export MANPAGER="less -JMR --use-color -Dd+r -Du+b -DPcs -DERd -DSys --mouse"

# ==============================================================================
# XDG Base Directories
# ==============================================================================
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"
export XDG_DATA_DIRS="/usr/share:/usr/local/share:$XDG_DATA_HOME"

# Logging & Session Files
export ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"
export LD_LIBRARY_PATH="/usr/local/lib/"
export XAUTHORITY="$HOME/.Xauthority"

# Shell & Config Directories
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export DOTFILES="$HOME/dotfiles"
export SVDIR="$XDG_CONFIG_HOME/service"

# ==============================================================================
# History & Cache Redirects
# ==============================================================================
export HISTFILE="$XDG_STATE_HOME/bash/history"
export NODE_REPL_HISTORY="$XDG_STATE_HOME/node_repl_history"
export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"
export PYTHON_HISTORY="$XDG_STATE_HOME/python_history"
export SQLITE_HISTORY="$XDG_STATE_HOME/sqlite_history"
export CALCHISTFILE="$XDG_CACHE_HOME/calc_history"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv"

# ==============================================================================
# Tool & Language Configurations
# ==============================================================================
# Android
export ANDROID_USER_HOME="$XDG_DATA_HOME/android"
alias adb='HOME="$XDG_DATA_HOME"/android adb'

# Go
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$GOPATH/bin"

# Haskell / GHCup / Stack
export GHCUP_USE_XDG_DIRS=true
export STACK_ROOT="$XDG_DATA_HOME/stack"
export STACK_XDG=1

# Node / NPM / PNPM / Bun
export PRETTIERD_DEFAULT_CONFIG="$XDG_CONFIG_HOME/prettier/.prettierrc"
export NPM_CONFIG_INIT_MODULE="$XDG_CONFIG_HOME/npm/config/npm-init.js"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NPM_CONFIG_TMP="${XDG_RUNTIME_DIR:-/tmp}/npm"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
export BUN_INSTALL="$HOME/.bun"

# Ruby Bundler
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"

# Helix
export HELIX_DEFAULT_RUNTIME="/usr/lib/helix/runtime"
export HELIX_RUNTIME="/usr/lib/helix/runtime"

# Other Tools
export VOID_PACKAGES="$XDG_DATA_HOME/void-packages/"
export ELM_HOME="$XDG_CONFIG_HOME/elm"
export ODIN_ROOT="/usr/local/share/odin"
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export TERMINFO="$XDG_DATA_HOME/terminfo"
export TERMINFO_DIRS="$XDG_DATA_HOME/terminfo:/usr/share/terminfo"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
export CARGO_HOME="$XDG_DATA_HOME/cargo"

# ==============================================================================
# PATH Construction (Duplicate-Safe)
# ==============================================================================

# Source GHCup early so custom prepend rules below take precedence
[ -f "$XDG_DATA_HOME/ghcup/env" ] && . "$XDG_DATA_HOME/ghcup/env"

# Helper function to append to PATH
add_to_path() {
    if [ -d "$1" ] && case ":$PATH:" in *":$1:"*) false;; *) true;; esac; then
        PATH="$PATH:$1"
    fi
}

# Helper function to prepend to PATH
prepend_to_path() {
    if [ -d "$1" ] && case ":$PATH:" in *":$1:"*) false;; *) true;; esac; then
        PATH="$1:$PATH"
    fi
}

# Apply PATH updates (High priority items PREPENDED to beat system paths like Plan 9)
add_to_path "$GOBIN"
add_to_path "$HOME/.bin"
prepend_to_path "$HOME/.local/sbin"
prepend_to_path "$HOME/.local/bin"

prepend_to_path "$CARGO_HOME/bin"
prepend_to_path "$PNPM_HOME/bin"
prepend_to_path "$BUN_INSTALL/bin"

# ASDF Shims
prepend_to_path "${ASDF_DATA_DIR:-$HOME/.asdf}/shims"

export PATH

# ==============================================================================
# Desktop, GUI & Input Methods
# ==============================================================================
export QT_QPA_PLATFORMTHEME="qt5ct"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export MPD_HOST="127.0.0.1"
export MPD_PORT="6600"

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export XCURSOR_SIZE=18

# Source theme/cursor environments if they exist
[ -f "$XDG_CONFIG_HOME/X11/xcursor_env" ] && . "$XDG_CONFIG_HOME/X11/xcursor_env"
[ -f "$XDG_DATA_HOME/global_theme_env" ] && . "$XDG_DATA_HOME/global_theme_env"

# ==============================================================================
# External Tool Environments & Shell Sourcing
# ==============================================================================
# Source bashrc if running Bash
if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi
