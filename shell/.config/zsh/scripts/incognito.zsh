# --- Incognito Mode Setup ---

# Idea Credit: https://twdev.blog/2026/02/bash_incognito/

# Define colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Set the original history path only if it hasn't been set yet
# This prevents it from being overwritten if you source .zshrc while in incognito
export OG_HISTFILE="${OG_HISTFILE:-${XDG_CONFIG_HOME:-$HOME/.config}/zsh/zsh_history}"

incognito() {
    if [ -n "$INCOGNITO_MODE" ]; then
        # Turning OFF incognito
        export HISTFILE="$OG_HISTFILE"
        setopt SHARE_HISTORY
        setopt INC_APPEND_HISTORY
        setopt APPEND_HISTORY
        unset INCOGNITO_MODE
        echo -e "${RED}INCOGNITO OFF (History recording resumed)${NC}"
    else
        # Turning ON incognito
        export HISTFILE=/dev/null
        unsetopt SHARE_HISTORY
        unsetopt INC_APPEND_HISTORY
        unsetopt APPEND_HISTORY
        export INCOGNITO_MODE=active
        echo -e "${GREEN}INCOGNITO ON (History is RAM-only)${NC}"
    fi
}
