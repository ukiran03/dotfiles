# =========================================================
# Plugins
# =========================================================

ZPLUGINDIR="${ZDOTDIR:-$HOME/.config/zsh}/plugins"

_zplugin_load() {
  # Split the input (e.g., "zsh-users/zsh-autosuggestions") by the slash
  local repo="${1%/*}"
  local name="${1#*/}"

  local plugin_path="${ZPLUGINDIR}/${name}"

  if [[ ! -d "$plugin_path" ]]; then
    mkdir -p "$ZPLUGINDIR"
    echo "Installing ${name}..."
    git clone --depth=1 "https://github.com/${repo}/${name}" "$plugin_path" \
      || { echo "ERROR: failed to install ${name}" >&2; return 1; }
  fi
  source "${plugin_path}/${name}.plugin.zsh"
}

zplugin-update() {
  local dir
  for dir in "${ZPLUGINDIR}"/*/; do
    echo "Updating ${dir:t}..."
    git -C "$dir" pull --ff-only
  done
}

# Load plugins using the single string "user/repo" format

_zplugin_load zsh-users/zsh-autosuggestions
_zplugin_load zsh-users/zsh-syntax-highlighting
_zplugin_load hlissner/zsh-autopair
# _zplugin_load marlonrichert/zsh-hist

# _zplugin_load zsh-users/zsh-history-substring-search

# _zplugin_load Aloxaf/fzf-tab
# TODO: Add histroy-substrig-search, zsh-hist, auto-notify, you-should-use

# Standalone plugin scripts
source /home/ukiran/.config/zsh/plugins/prezto-completions.zsh
