#### Paths
# all alias are in .config/shell/aliasrc
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
fi

if [ -d "$HOME/.bin" ]; then
    PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

## fzf

# export FZF_CTRL_R_OPTS='--prompt "Shell: " --info inline-right --height=35%'
export FZF_CTRL_R_OPTS=' --prompt "Search History: " --bind "enter:become:if [ -z {} ]; then echo {q}; else echo {}; fi"'


# Preview file content using bat (https://github.com/sharkdp/bat)
export FZF_CTRL_T_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'bat -n --color=always {}'
  --bind 'ctrl-/:change-preview-window(down|hidden|)'"

# Print tree structure in the preview window
export FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'ls --color=always --group-directories-first -F {}'"

export FZF_DEFAULT_OPTS='--cycle --reverse --color 16 --height=~40% --border=bold --prompt "Search: " --info inline-right'

# SHELL ZSH
export HISTCONTROL=ignoreboth:erasedups # Both `ignoredups` and `erasedups` options are enabled
export HISTDUP=erase                    # Duplicate history entries should be erased from the history list
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE

# export MANPAGER="most"
export GROFF_NO_SGR=1
export MANPAGER="less -JMR --use-color -Dd+r -Du+b"

# default apps
# export EDITOR="nvim"
# export EDITOR="emacsclient -nw -a 'emacs'"
export EDITOR="emacs -q --load ~/.mini-emacs.el -nw"
export VISUAL="emacs -q --load ~/.mini-emacs.el -nw"
export TERMINAL="alacritty"
export BROWSER="firefox"
# export VISUAL="emacsclient -c -a 'emacs'"

# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# helix bug in void-src, so manual:
export HELIX_DEFAULT_RUNTIME=/usr/lib/helix/runtime
export HELIX_RUNTIME=/usr/lib/helix/runtime

# # default folders
# export XDG_CONFIG_HOME="$HOME/.config"
# export XDG_DATA_HOME="$HOME/.local/share"
# export XDG_STATE_HOME="$HOME/.local/state"
# export XDG_CACHE_HOME="$HOME/.cache"
# export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"
# export XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"

# Dotfiles
# export DOTFILES="$HOME/dotfiles"

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

export PNPM_HOME="/home/ukiran/.local/share/pnpm"
if [[ -d "$PNPM_HOME" && ! ":$PATH:" == *":$PNPM_HOME:"* ]]; then
  export PATH="$PNPM_HOME:$PATH"
fi
## plugins: fzf, zoxide
# zoxide
eval "$(zoxide init zsh)"
## Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.crdownload=00;90:*.dpkg-dist=00;90:*.dpkg-new=00;90:*.dpkg-old=00;90:*.dpkg-tmp=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:*.swp=00;90:*.tmp=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:';


nnn ()
{
    # Block nesting of nnn in subshells
    [ "${NNNLVL:-0}" -eq 0 ] || {
        echo "nnn is already running"
        return
    }

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
    # see. To cd on quit only on ^G, remove the "export" and make sure not to
    # use a custom path, i.e. set NNN_TMPFILE *exactly* as follows:
    #      NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    # The command builtin allows one to alias nnn to n, if desired, without
    # making an infinitely recursive alias
    command nnn "$@"

    [ ! -f "$NNN_TMPFILE" ] || {
        . "$NNN_TMPFILE"
        rm -f -- "$NNN_TMPFILE" > /dev/null
    }
}
