# ==============================================================================
# ENVIRONMENT & CORE PREFERENCES
# ==============================================================================
export COLORTERM="truecolor"
export STARSHIP_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/starship/starship.toml"
export WORDCHARS='*?_[]~=&;!#$%^(){}<>' # Removed: -./ for better word navigation

# Define ZDOTDIR fallback safely
: "${ZDOTDIR:=${XDG_CONFIG_HOME:-$HOME/.config}/zsh}"

# Load Colors module
autoload -U colors && colors

# Disable Ctrl+S terminal freeze
stty stop undef 2>/dev/null

# ==============================================================================
# ZSH OPTIONS
# ==============================================================================
# Directory Navigation
setopt AUTOCD            # Automatically cd into typed directory name
setopt AUTO_PUSHD        # Push directory to stack on cd
setopt PUSHD_IGNORE_DUPS # Do not store duplicate directories in stack
setopt PUSHD_SILENT      # Do not print directory stack after pushd/popd
setopt CORRECT           # Auto-correct mistyped commands

# Globbing
setopt NO_CASE_GLOB  # Case-insensitive globbing
setopt GLOB_DOTS     # Include hidden files in globbing
setopt EXTENDED_GLOB # Extended pattern matching features

# Line & Completion Behavior
setopt COMPLETE_ALIASES     # Complete aliases
setopt ALWAYS_TO_END        # Move cursor to end of word after completion
setopt AUTO_MENU            # Automatically display completion menu
setopt COMPLETE_IN_WORD     # Allow completion from within a word
setopt INTERACTIVE_COMMENTS # Allow comments in interactive shell
setopt PROMPT_SUBST         # Enable prompt variable substitution

# History Management
setopt SHARE_HISTORY          # Share history across all active sessions
setopt APPEND_HISTORY         # Preserve commands from all sessions
setopt INC_APPEND_HISTORY     # Save commands to history file immediately
setopt HIST_IGNORE_ALL_DUPS   # Delete old duplicate command if new one is typed
setopt HIST_IGNORE_SPACE      # Ignore commands starting with a space
setopt HIST_SAVE_NO_DUPS      # Do not write duplicate events to history file
setopt HIST_EXPIRE_DUPS_FIRST # Remove duplicates first when history is full
setopt HIST_FIND_NO_DUPS      # Do not display duplicates when searching history
setopt HIST_VERIFY            # Load expanded history entry into editing buffer first

# ==============================================================================
# KEY BINDINGS & LINE EDITOR (ZLE)
# ==============================================================================
bindkey -e # Use Emacs keybindings

bindkey '^[[3~' delete-char
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey '^H' backward-kill-word
bindkey '^U' backward-kill-line
bindkey '^[[3;5~' kill-word
bindkey '^_' undo
bindkey '^[[63;6~' redo

# Multi-line Command Line Editing (Ctrl-X Ctrl-E)
autoload -U edit-command-line
edit-command-line-and-redraw-prompt () {
    zle edit-command-line
    zle reset-prompt # redraw to fix mess of prompt and input text.
    zle end-of-buffer-or-history # move cursor to the end of input.
}
# load both widgets
zle -N edit-command-line
zle -N edit-command-line-and-redraw-prompt

# Use Ctrl-x-Ctrl-e to edit command line
bindkey '^x^e' edit-command-line-and-redraw-prompt

# ==============================================================================
# COMPLETIONS (Fast Init)
# ==============================================================================
autoload -U compinit
zmodload zsh/complist

# Perform compinit check only once a day for fast startup time
if [[ -n "${ZDOTDIR}/.zcompdump(#qN-m+1)" ]]; then
	compinit
else
	compinit -C
fi

_comp_options+=(globdots) # Include hidden files in completions
zstyle ':completion:*' menu select

# Match aliases for completion wrapper
compdef ,g=git
compdef ,d=docker

# ==============================================================================
# SOURCE EXTERNAL CONFIGURATIONS & PLUGINS
# ==============================================================================
# Primary Exports
if [[ -f "$ZDOTDIR/exports.zsh" ]]; then
	source "$ZDOTDIR/exports.zsh"
else
	echo "Warning: $ZDOTDIR/exports.zsh missing"
fi

# Zsh Plugins
[[ -f "$ZDOTDIR/plugins.zsh" ]] && source "$ZDOTDIR/plugins.zsh"

# Incognito helper script
[[ -f "$ZDOTDIR/scripts/incognito.zsh" ]] && source "$ZDOTDIR/scripts/incognito.zsh"

# ==============================================================================
# INTEGRATIONS & TOOLS
# ==============================================================================

# Custom Widgets

# Copy current command buffer to clipboard (Linux / X11)
function copy-buffer-to-clipboard() {
  echo -n "$BUFFER" | xclip -selection clipboard
  zle -M "Copied to clipboard"
}
zle -N copy-buffer-to-clipboard
bindkey '^Xy' copy-buffer-to-clipboard

# Aliases
alias rm="trash-put -v"

# Suffix Aliases
alias -s md=bat # Just type the filename to open it with the associated program

# Tmux Pane Rename Helper
rename_tmux_pane_to_cwd() {
	if [[ -n "$TMUX" ]]; then
		tmux rename-window "$(basename "$PWD")"
	else
		echo "Not in a tmux session."
	fi
}

# Direnv
if command -v direnv &>/dev/null; then
	eval "$(direnv hook zsh)"
fi

# Starship Prompt
if command -v starship &>/dev/null; then
	eval "$(starship init zsh)"
fi

# source /home/ukiran/dotfiles/shell/.config/zsh/completions/_kubectl

# Google Cloud SDK
GCLOUD_SDK_DIR="$HOME/.local/gke/google-cloud-sdk"

if [[ -d "$GCLOUD_SDK_DIR" ]]; then
	[[ -f "$GCLOUD_SDK_DIR/path.zsh.inc" ]] &&
		source "$GCLOUD_SDK_DIR/path.zsh.inc"
	[[ -f "$GCLOUD_SDK_DIR/completion.zsh.inc" ]] &&
		source "$GCLOUD_SDK_DIR/completion.zsh.inc"
fi
