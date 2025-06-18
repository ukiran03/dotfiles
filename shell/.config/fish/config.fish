if status is-interactive
    # Commands to run in interactive sessions can go here
    set -g fish_greeting

    # Abbrs
    abbr ,g 'git'
    abbr ,gs 'git status'
    abbr ,gl 'git log --oneline --graph --all'
    abbr ,gp 'git pull'
    abbr ,gcm 'git commit'
    abbr ,gcl 'git clone'

    abbr cp "cp -iv"
    abbr mv "mv -iv"
    abbr rm "rm -iv"
    abbr mkdir "mkdir -pv"

    alias ls='eza --color=always --group-directories-first -F'
    alias ll="eza --color=always --group-directories-first -lF"
    alias la="eza --color=always --group-directories-first -aF"
    alias lal="eza --color=always --group-directories-first -laF"

    # abbr ls 'eza --color=always --group-directories-first -F'
    # abbr ll "eza --color=always --group-directories-first -lF"
    # abbr la "eza --color=always --group-directories-first -aF"
    # abbr lal "eza --color=always --group-directories-first -laF"

    abbr rso "redshift -O"
    abbr rsx "redshift -x"

    abbr nsxiv "nsxiv -o"

    abbr emacsc "emacsclient -c"
    abbr emacsn "emacsclient -nw"

    # displays
    abbr monmain "xrandr --output HDMI-1-0 --auto --primary & xrandr --output eDP-1 --off"
    abbr lapmain "xrandr --output eDP-1 --auto --primary & xrandr --output HDMI-1-0 --off"
    abbr 2heads "xrandr --output HDMI-1-0 --auto --primary --output eDP-1 --auto --left-of HDMI-1-0"

    abbr cdu 'cdu -idh'
    abbr diff 'diff --color'
    abbr df 'df -h'

    abbr scaps "setxkbmap -option"
    abbr nocaps "setxkbmap -option ctrl:nocaps"

    export MANPAGER="less -JMR --use-color -Dd+r -Du+b"
    export FZF_DEFAULT_OPTS='--reverse --color 16 --height=50% --border=bold --prompt "Search: " --info inline-right'
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_STATE_HOME="$HOME/.local/state"
    export XDG_CACHE_HOME="$HOME/.cache"
    export XDG_SCREENSHOTS_DIR="$HOME/Pictures/screenshots"
    export XDG_DATA_DIRS="/usr/share:/usr/local/share:$HOME/.local/share"


    zoxide init fish | source
end
