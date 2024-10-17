#### Autoload
# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
eval "$(zoxide init zsh)"	# zoxide
eval "$(fzf --zsh)"		# history with fzf (C-r)

#### Prompt
# PROMPT="%F{#41fdfe}[%n %F{#FF6188}%?%f %F{cyan}%~%f%F{#41fdfe}]%f$ "  # Dark theme
# PROMPT="%F{blue}[%n %F{red}%?%f %F{cyan}%~%f%F{blue}]%f$ "  # Light theme

# PROMPT="%F{cyan}[%f %F{cyan}%~%f%F{blue} %B%F{#FF6188}%?%f%b %F{cyan}]%f %B%F{yellow}∮%b "  # Dark theme
PROMPT="%F{#41fdfe}[%f %F{cyan}%~%f%F{#41fdfe} %B%F{#FF6188}%?%f%b %F{#41fdfe}]%f %B%F{yellow}∮%f%b "  # Dark theme

    # "∮"
    # "∯"
    # "≎"
    # ""
    # ""
    # ""
    # ""
    # ""
    # ""

RPROMPT='%F{006}$(parse_git_branch)'

# git branch on right prompt
function parse_git_dirty {
  STATUS="$(git status 2> /dev/null)"
  if [[ $? -ne 0 ]]; then printf ""; return; else printf " ["; fi
  if echo ${STATUS} | grep -c "renamed:"         &> /dev/null; then printf " >"; else printf ""; fi
  if echo ${STATUS} | grep -c "branch is ahead:" &> /dev/null; then printf " !"; else printf ""; fi
  if echo ${STATUS} | grep -c "new file::"       &> /dev/null; then printf " +"; else printf ""; fi
  if echo ${STATUS} | grep -c "Untracked files:" &> /dev/null; then printf " ?"; else printf ""; fi
  if echo ${STATUS} | grep -c "modified:"        &> /dev/null; then printf " *"; else printf ""; fi
  if echo ${STATUS} | grep -c "deleted:"         &> /dev/null; then printf " -"; else printf ""; fi
  printf " ]"
}
function parse_git_branch() {
  # Long form
  git rev-parse --abbrev-ref HEAD 2> /dev/null
 # Short form
  # git rev-parse --abbrev-ref HEAD 2> /dev/null | sed -e 's/.*\/\(.*\)/\1/'
}

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## fzf
export FZF_CTRL_R_OPTS='--reverse --border=bold --border-label="Shell History" --prompt "Search: " --info inline-right'

export FZF_DEFAULT_OPTS='--reverse --height=75% --border=bold --prompt "Search: " --info inline-right'

#### Options
setopt autocd		         # Automatically cd into typed directory.
setopt NO_CASE_GLOB	         # Modifies the behavior of filename globbing
			         # (pattern matching) to be case-insensitive
setopt COMPLETE_ALIASES	         # Perform command completion for aliases
setopt GLOB_DOTS	         # Include hidden files when performing file globbing
setopt SHARE_HISTORY	         # Share history between all sessions.
setopt prompt_subst	         # Enables prompt substitution
setopt always_to_end	         # Ensures the cursor moves to the end of the command line
setopt APPEND_HISTORY	         # Preserving the commands from all sessions

setopt auto_menu	         # Automatically displays a menu of possible completions
setopt complete_in_word	         # Attempt to complete the word at the cursor position
setopt hist_verify	         # Re-evaluate the modified command line before executing it
setopt INC_APPEND_HISTORY	 # Incremental history writing to hist_file

setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
# setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_EXPIRE_DUPS_FIRST    # Prioritize removing duplicate events over non-duplicate ones
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.

export HISTCONTROL=ignoreboth:erasedups # Both `ignoredups` and `erasedups` options are enabled
export HISTDUP=erase	   	 # Duplicate history entries should be erased from the history list
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE
export HISTFILE="/home/ukiran/.config/zsh/zsh_history"

export EDITOR='vim'
export VISUAL="emacsclient -c -a 'emacs'"
export LANG=en_US.UTF-8

#### Paths

# all alias are in .config/shell/aliasrc
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
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
#--------------------multi line edit
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
#--------------------multi line edit

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
source $HOME/.local/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.local/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.local/share/zsh/plugins/zsh-completions/zsh-completions.plugin.zsh
