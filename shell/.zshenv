# default apps
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox"

# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Set PATH so it includes user's private executables.
if [ -d "$HOME"/.bin ]; then
    PATH=$PATH:"$HOME"/.bin
fi

# This is another possible location for user-specific binaries.
if [ -d "$HOME"/.local/bin ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi



# default folders
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"

# Dotfiles
export DOTFILES="$HOME/dotfiles"

# Bash History
# export HISTFILE="$XDG_CONFIG_HOME/shell/history"
if [ "$SHELL" = "/bin/zsh" ]; then
    export HISTFILE="$XDG_CONFIG_HOME/zsh/zsh_history"
else
    export HISTFILE="$XDG_CONFIG_HOME/shell/bash_history"
fi

# wget
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# Cuda
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv

export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc

# Ipython
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"

# libice
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority

# Cargo :Rust
export CARGO_HOME="$XDG_DATA_HOME"/cargo

# GOPATH :Go
export GOPATH="$XDG_DATA_HOME"/go

# set the localization
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
