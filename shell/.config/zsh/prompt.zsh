function get_status_color() {
	local exit_status=$? # <--- CRUCIAL: Capture $? immediately
	[[ $exit_status == 0 ]] && echo "%B%F{002}$%f%b" || echo "%F{009}($exit_status) $%f"
}

NEWLINE=$'\n'
JOBS=$'%(1j.%F{003}[%j] %f.)'
STATUS=$'$get_status_color'
USERP=$'%B%F{012}%n@%m%f%b'
UPWD=$'%B%F{006}%K%~%k%f%b'
PUPWD=$':: %B%F{006}%~%f%b'
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

if [[ -n $TMUX ]] || [[ $TERM == rxvt-unicode-256color ]]; then
	PROMPT='${JOBS}${USERP} ${UPWD} %F{008}$vcs_info_msg_0_%f ${NEWLINE}$(get_status_color) '
else
	PROMPT='${JOBS}${USERP} ${PUPWD} %F{008}$vcs_info_msg_0_%f ${NEWLINE}$(get_status_color) '
fi


# PROMPT='${USERP} ${PUPWD} %F{008}$vcs_info_msg_0_%f ${NEWLINE}$(get_status_color) '
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

prompt-k8s() {
	source ~/.config/zsh/plugins/kube-ps1/kube-ps1.sh
	KUBE_PS1_SYMBOL_ENABLE=false
    KUBE_PS1_PREFIX=''
	KUBE_PS1_SUFFIX=''
	PROMPT='${JOBS}${USERP} ${UPWD} %F{008}$vcs_info_msg_0_%f $(kube_ps1)${NEWLINE}$(get_status_color) '
	# Optional: Re-initialize your prompt if the script doesn't do it automatically
	# zle reset-prompt 2>/dev/null
}
