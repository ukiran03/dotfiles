#### Paths
# all alias are in .config/shell/aliasrc
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] ;
then source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
fi

if [ -d "$HOME/.bin" ] ;
then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
then PATH="$HOME/.local/bin:$PATH"
fi

## plugins: fzf, zoxide
# zoxide
eval "$(zoxide init zsh)"
## Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
## fzf
export FZF_CTRL_R_OPTS='--prompt "Shell: " --info inline-right --height=35%'

# Preview file content using bat (https://github.com/sharkdp/bat)
export FZF_CTRL_T_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'bat -n --color=always {}'
  --bind 'ctrl-/:change-preview-window(down|hidden|)'"

# Print tree structure in the preview window
export FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'ls --color=always --group-directories-first -F {}'"

export FZF_DEFAULT_OPTS='--reverse --color 16 --height=50% --border=bold --prompt "Search: " --info inline-right'


# SHELL ZSH
export HISTCONTROL=ignoreboth:erasedups # Both `ignoredups` and `erasedups` options are enabled
export HISTDUP=erase	   	     # Duplicate history entries should be erased from the history list
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE

# export MANPAGER="most"
export GROFF_NO_SGR=1
export MANPAGER="less -JMR --use-color -Dd+r -Du+b"

# default apps
# export EDITOR="nvim"
export EDITOR="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox"
# export VISUAL="emacsclient -c -a 'emacs'"


# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# helix bug in void-src, so manual:
export HELIX_DEFAULT_RUNTIME=/usr/lib/helix/runtime
export HELIX_RUNTIME=/usr/lib/helix/runtime

# default folders
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"
export XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"

# Dotfiles
export DOTFILES="$HOME/dotfiles"

if [[ "$SHELL" = "/bin/zsh" || "$SHELL" = "/usr/bin/zsh" ]]; then
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
#export GOPATH="$XDG_DATA_HOME"/go
#export GOBIN="$GOPATH"/bin
#export PATH=$PATH:$GOPATH/bin

# set the localization
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
