# .bashrc

#PS1='\[\e[38;5;33;1m\][\[\e[0m\] \[\e[38;5;51m\]\w\[\e[0m\] $? \[\e[38;5;33;1m\]]\[\e[0m\] \[\e[38;5;220;1m\]\\$\[\e[0m\] '
#PS2='> '

#PS1='\[\e[1;34m\]\u@\h \[\e[1;32m\]\w\[\e[0m\] \n$? $ '
PS1='\[\e[1;34m\]\u@\h \[\e[1;32m\]\w\[\e[0m\] \n\[\e[1;31m\]$? \[\e[0m\]$ '



# # If not running interactively, don't do anything
[[ $- != *i* ]] && return



# if [[ $- == *i* ]]; then
#   # Add your bind commands here
#   eval "$(fzf --bash)"
#   eval "$(zoxide init bash)"

# fi

alias ls='ls --color=auto'

source /home/ukiran/.config/broot/launcher/bash/br
