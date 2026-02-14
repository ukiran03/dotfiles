# .bashrc


PS1='\[\e[1;34m\]\u@\h \[\e[1;32m\]\w\[\e[0m\] \n\[\e[1;31m\]$? \[\e[0m\]$ '


# # If not running interactively, don't do anything
[[ $- != *i* ]] && return



if [[ $- == *i* ]]; then
#   eval "$(fzf --bash)"
  eval "$(zoxide init bash)"
fi

alias ls='eza --color -F --group-directories-first'

# source /home/ukiran/.config/broot/launcher/bash/br
# eval "$(mise activate bash)"
#. "/home/ukiran/.deno/env"

. "$HOME/.atuin/bin/env"

[[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh
eval "$(atuin init bash)"
