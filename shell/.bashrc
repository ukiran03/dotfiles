# .bashrc

export PS1='\[\e[38;5;51m\][\[\e[0m\] \[\e[38;5;39m\]\w\[\e[0m\] \[\e[38;5;198;1m\]$?\[\e[0m\] \h \[\e[38;5;51m\]]\[\e[0m\] \\$ '
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
