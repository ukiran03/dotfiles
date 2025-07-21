#### Autoload
# Enable colors and change prompt:
autoload -U colors && colors # Load colors

function get_status_color() {
    local exit_status=$? # <--- CRUCIAL: Capture $? immediately
    [[ $exit_status == 0 ]] && echo "%B%F{002}● $%f%b" || echo "%F{009}($exit_status) $%f"
}

NEWLINE=$'\n'
STATUS=$'$get_status_color'
USER=$'%B%F{012}%n@%F{015}%m%f%f%b'
UPWD=$'%B%F{006}%K%~%k%f%b'

PROMPT='${USER} ${UPWD} %F{007}$vcs_info_msg_0_%f ${NEWLINE}$(get_status_color) '

# export REPORTTIME=3
TIMEFMT="'$fg[green]%J$reset_color' time: $fg[blue]%*Es$reset_color, cpu: $fg[blue]%P$reset_color"

function preexec() {
    timer=${timer:-$SECONDS}
}
function precmd() {
    if [ $timer ]; then
        timer_show=$(($SECONDS - $timer))
        if [ $timer_show -ge 3 ]; then
            RPROMPT="%F{cyan}${timer_show}s %{$reset_color%}"
        else
            RPROMPT=""
        fi
        unset timer
    fi
}

### VC Info
# Autoload zsh's `add-zsh-hook` and `vcs_info` functions
# (-U autoload w/o substition, -z use zsh style)
autoload -Uz add-zsh-hook vcs_info

# Run the `vcs_info` hook to grab git info before displaying the prompt
add-zsh-hook precmd vcs_info

# Style the vcs_info message
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats '(%b%u%c)'

zstyle ':vcs_info:git*' unstagedstr ' %F{009}*%f'
zstyle ':vcs_info:git*' stagedstr '%F{010}+%f'
## This enables %u and %c (unstaged/staged changes) to work,
## but can be slow on large repos
zstyle ':vcs_info:*:*' check-for-changes false # true

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

# Zsh Directory Stack
setopt AUTO_PUSHD        # Push the current directory visited on the stack.
setopt PUSHD_IGNORE_DUPS # Do not store duplicates in the stack.
setopt PUSHD_SILENT      # Do not print the directory stack after pushd or popd.

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

#### Completions
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' # case insenitive
# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # Include hidden files.

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

#### Plugins
# should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# ZSH AutoPair Plugin
source /home/ukiran/.config/zsh/plugins/zsh-autopair/autopair.zsh
autopair-init

# Jump back to a specific directory
source /home/ukiran/.config/zsh/plugins/bd/bd.zsh

[ -f "/home/ukiran/.ghcup/env" ] && . "/home/ukiran/.ghcup/env" # ghcup-env
source /home/ukiran/.config/broot/launcher/bash/br
