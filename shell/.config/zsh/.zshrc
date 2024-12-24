#### Autoload
# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
eval "$(zoxide init zsh)"	# zoxide

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
# eval "$(batman --export-env)"
PROMPT="%B%F{014}[%f %F{012}%~%f %F{009}%?%f %F{014}]%f %F{011}∮%f%b "
RPROMPT='%F{07}⎇ $vcs_info_msg_0_%f'

### VC Info
# Autoload zsh's `add-zsh-hook` and `vcs_info` functions
# (-U autoload w/o substition, -z use zsh style)
autoload -Uz add-zsh-hook vcs_info

# Run the `vcs_info` hook to grab git info before displaying the prompt
add-zsh-hook precmd vcs_info

# Style the vcs_info message
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats '%b%u%c'
# Format when the repo is in an action (merge, rebase, etc)
zstyle ':vcs_info:git*' actionformats '%F{14} %*%f'
zstyle ':vcs_info:git*' unstagedstr '*'
zstyle ':vcs_info:git*' stagedstr '+'
# This enables %u and %c (unstaged/staged changes) to work,
# but can be slow on large repos
zstyle ':vcs_info:*:*' check-for-changes true

# export MANPAGER="most"
export GROFF_NO_SGR=1
export MANPAGER="less -R --use-color -Dd+r -Du+b"


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## fzf
export FZF_CTRL_R_OPTS='--reverse --border=bold --border-label="Shell History" --prompt "Search: " --info inline-right'

# Preview file content using bat (https://github.com/sharkdp/bat)
export FZF_CTRL_T_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'bat -n --color=always {}'
  --bind 'ctrl-/:change-preview-window(down|hidden|)'"

# Print tree structure in the preview window
export FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'tree -C {}'"

export FZF_DEFAULT_OPTS='--reverse --height=75% --border=bold --prompt "Search: " --info inline-right'

#### Options
setopt autocd		             # Automatically cd into typed directory.
setopt NO_CASE_GLOB	             # Modifies the behavior of filename globbing
                                 # (pattern matching) to be case-insensitive
setopt COMPLETE_ALIASES	         # Perform command completion for aliases
setopt GLOB_DOTS	             # Include hidden files when performing file globbing
setopt SHARE_HISTORY	         # Share history between all sessions.
setopt prompt_subst	             # Enables prompt substitution
setopt always_to_end	         # Ensures the cursor moves to the end of the command line
setopt APPEND_HISTORY	         # Preserving the commands from all sessions

setopt auto_menu	             # Automatically displays a menu of possible completions
setopt complete_in_word	         # Attempt to complete the word at the cursor position
setopt hist_verify	             # Re-evaluate the modified command line before executing it
setopt INC_APPEND_HISTORY	     # Incremental history writing to hist_file

setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
# setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_EXPIRE_DUPS_FIRST    # Prioritize removing duplicate events over non-duplicate ones
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.

export HISTCONTROL=ignoreboth:erasedups # Both `ignoredups` and `erasedups` options are enabled
export HISTDUP=erase	   	     # Duplicate history entries should be erased from the history list
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE
export HISTFILE="/home/ukiran/.config/zsh/zsh_history"

export EDITOR='nvim'
export VISUAL="emacsclient -c -a 'emacs'"
export LANG=en_US.UTF-8

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

### Functions
backupthis ()
{
    cp -riv $1 ${1}-$(date +%Y%m%d%H%M).backup;
}

#### Keybinds
bindkey -e
bindkey "^[[3~" delete-char
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey '^H' backward-kill-word
bindkey '^U' backward-kill-line
bindkey '^[[3;5~' kill-word
# bindkey '^[[63~' undo
bindkey '^_' undo
# Bind Ctrl+Shift+/ to redo
bindkey '^[[63;6~' redo
stty stop undef		# Disable ctrl-s to freeze terminal.

### Multi line edit
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

#### Completions
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' # case insenitive
# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.


#### Plugins
# Load syntax highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
#source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

#source $HOME/.local/share/zsh/plugins/zsh-completions/zsh-completions.plugin.zsh

### Icons and Colors
# ∮ ∯ ≎           󱑌(clock)
