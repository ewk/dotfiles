#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load git completion engine
source /usr/share/git/completion/git-prompt.sh
#GIT_PS1_SHOWDIRTYSTATE=true  	# + for staged, * if unstaged.
#GIT_PS1_SHOWSTASHSTATE=true	# $ if something is stashed.
#GIT_PS1_SHOWUNTRACKEDFILES=true	# % if there are untracked files.
#GIT_PS1_SHOWUPSTREAM 	<,>,<> behind, ahead, or diverged from upstream.
GIT_PS1_SHOWCOLORHINTS=true # Option for git-prompt.sh to show branch name in color

# Enable the useful shell completions that should have been the default
source /usr/share/doc/pkgfile/command-not-found.bash
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'
bind 'set completion-ignore-case on'
bind 'set completion-map-case on'

# The prompt string is really quite gnarly.
# The color codes are verbose, and spacing is tight to get the prompt to look right.
# \e[38;5;178m is the foreground color code.
# \e[0m resets the color code
# For example, this part changes a single '[' purple:
#	\e[38;5;54m[\e[0m
# A road map to the displayed format strings is included below.
PS1='\u@\h \e[38;5;178m\W\e[0m$(__git_ps1 " \e[38;5;54m[\e[0m\e[38;5;70m%s\e[0m\e[38;5;54m]\e[0m")$(parse_git_stash) \$ '
#     ^  ^              ^                              ^                ^^                ^                           ^

alias ls='ls --color=auto'
alias grep='egrep --color --ignore-case'
alias emacsclient='emacsclient --no-wait'
alias ll='ls -ahlF'
alias gpg=gpg2
alias open="xdg-open $1"

# History
PROMPT_COMMAND='history -a' # Record each line as it gets issued
# bind up and down arrow keys to search through history
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
shopt -s histappend # Append to the history file, don't overwrite it
HISTCONTROL=ignoreboth # ignore duplicate commands and leading spaces
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear:reset:"

# Change prompt if a Git repo has stashed changes on the stack
parse_git_stash () {
	[[ $(git stash list 2> /dev/null | tail -n1) != "" ]] && echo -e '\U1F95E '
}

# Unpack any archive
# See also: xkcd.com/1168/
extract () {
	if [ -f $1 ] ; then
		case $1 in
			*.tar.bz2)   tar xvjf $1    ;;
			*.tar.gz)    tar xvzf $1    ;;
			*.bz2)       bunzip2 $1     ;;
			*.rar)       unrar x $1       ;;
			*.gz)        gunzip $1      ;;
			*.tar)       tar xvf $1     ;;
			*.tbz2)      tar xvjf $1    ;;
			*.tgz)       tar xvzf $1    ;;
			*.zip)       unzip $1       ;;
			*.Z)         uncompress $1  ;;
			*.7z)        7z x $1        ;;
			*)           echo "don't know how to extract '$1'..." ;;
		esac
	else
		echo "'$1' is not a valid file!"
	fi
}

# EDITOR
export EDITOR="gvim -f"
export SUDO_EDITOR="vim"
export CSCOPE_EDITOR="vim"

# Colorful man pages
man() {
	LESS_TERMCAP_mb=$'\E[01;31m' \
	LESS_TERMCAP_md=$'\E[01;38;5;74m' \
	LESS_TERMCAP_me=$'\E[0m' \
	LESS_TERMCAP_se=$'\E[0m' \
	LESS_TERMCAP_so=$'\E[38;5;246m' \
	LESS_TERMCAP_ue=$'\E[0m' \
	LESS_TERMCAP_us=$'\E[04;38;5;146m' \
	command man "$@"
}

# Sets the Mail Environment Variable
MAIL=/var/spool/mail/ewk && export MAIL

# Ack settings
ACKRC=~/.ackrc

# Grep settings
GREP_COLOR='1;30;43'

# GPG config - will only ask for password once per shell
keychain --eval --quiet >/dev/null

# OPTIONS
set -o noclobber # don't overwrite existing files
shopt -s cmdhist # Save multi-line commands as one command
shopt -s autocd # type dir to chdir
shopt -s checkwinsize # update lines and columns when window size changes
