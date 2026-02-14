#### Autoload
autoload -U colors && colors

#### Options
setopt autocd       # Automatically cd into typed directory.
setopt NO_CASE_GLOB # Modifies the behavior of filename globbing
# (pattern matching) to be case-insensitive
setopt COMPLETE_ALIASES # Perform command completion for aliases
setopt GLOB_DOTS        # Include hidden files when performing file globbing
setopt SHARE_HISTORY    # Share history between all sessions.
setopt PROMPT_SUBST     # Enables prompt substitution
setopt always_to_end    # Ensures the cursor moves to the end of the command line
setopt APPEND_HISTORY   # Preserving the commands from all sessions
setopt INTERACTIVE_COMMENTS
setopt auto_menu          # Automatically displays a menu of possible completions
setopt complete_in_word   # Attempt to complete the word at the cursor position
setopt hist_verify        # Re-evaluate the modified command line before executing it
setopt INC_APPEND_HISTORY # Incremental history writing to hist_file

setopt HIST_IGNORE_ALL_DUPS # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_SPACE    # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS    # Do not write a duplicate event to the history file.
# setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_EXPIRE_DUPS_FIRST # Prioritize removing duplicate events over non-duplicate ones
setopt HIST_FIND_NO_DUPS      # Do not display a previously found event.
setopt HIST_IGNORE_DUPS       # Do not record an event that was just recorded again.
setopt EXTENDED_GLOB          # Globing
# Zsh Directory Stack
setopt AUTO_PUSHD        # Push the current directory visited on the stack.
setopt PUSHD_IGNORE_DUPS # Do not store duplicates in the stack.
setopt PUSHD_SILENT      # Do not print the directory stack after pushd or popd.
setopt CORRECT
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
stty stop undef # Disable ctrl-s to freeze terminal.

### Multi line edit
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

## Words deletion
export WORDCHARS='*?_[]~=&;!#$%^(){}<>' ## removed: -./

#### Completions
# zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' # case insenitive  ## prezto
# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # Include hidden files.
# set list-colors to enable filename colorizing
# zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

## Sources

if [[ -f $ZDOTDIR/exports.zsh ]]; then
	source $ZDOTDIR/exports.zsh
else
	echo "zsh-exports file missing"
fi

# if [[ -f $ZDOTDIR/zsh-funcs ]]; then
#   source $ZDOTDIR/zsh-funcs
# else
#   echo "zsh-funcs file missing"
# fi

# Rename the current tmux pane to the current working directory
rename_tmux_pane_to_cwd() {
	if [[ -n "$TMUX" ]]; then
		# Get the current pane's working directory and rename the pane
		tmux rename-window "$(basename "$PWD")"
	else
		echo "Not in a tmux session."
	fi
}

## Trash-Cli
alias rm="trash-put -v"
# alias rm="gtrash put"

## only for zsh
compdef ,g=git

## https://superuser.com/questions/649635/zsh-says-no-matches-found-when-trying-to-download-video-with-youtube-dl
# autoload -Uz bracketed-paste-magic
# zle -N bracketed-paste bracketed-paste-magic
# autoload -Uz url-quote-magic
# zle -N self-insert url-quote-magic

# https://www.jefftk.com/p/logging-shell-history-in-zsh
precmd() {
	echo "$(date +%Y-%m-%d--%H-%M-%S) $(hostname) $PWD $(history -1)" \
		>>~/.local/.full_history
}

function histgrep {
  local n_lines=10
  if [[ "$1" =~ ^[0-9]*$ ]]; then
    n_lines="$1"
    shift
  fi
  grep "$@" ~/.full_history | tail -n "$n_lines"
}

#### Plugins
# should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# ZSH AutoPair Plugin
source /home/ukiran/.config/zsh/plugins/zsh-autopair/autopair.zsh
autopair-init

source ~/.config/zsh/plugins/prezto-completions.zsh

### It have kubens, kubectx
# fpath=(~/.config/zsh/completions $fpath)
# autoload -U compinit && compinit

# . "$HOME/.atuin/bin/env"
# eval "$(atuin init zsh)"

cowsay "Learning by doing!"

eval "$(direnv hook zsh)"

source /home/ukiran/dotfiles/shell/.config/zsh/completions/_kubectl

#### Prompt
# source /home/ukiran/.config/zsh/prompt.zsh # Custom Prompt
eval "$(starship init zsh)" # Starship Prompt
