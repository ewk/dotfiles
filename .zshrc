# .zshrc main configuration file
# This file is sourced in interactive shells.
# Used to set up aliases, functions, options, key bindings and
# executing commands

#
# Standard shell
#

# User functions
alias grep='egrep --color --ignore-case'
alias emacsclient='emacsclient --no-wait'
alias ll='ls -ahlF'
alias gpg=gpg2
alias ls='ls --color=auto'

open() {
	command xdg-open $1
}

# Sets the Mail Environment Variable
MAIL=/var/spool/mail/ewk && export MAIL

# Grep settings
GREP_COLOR='1;30;43'

# GPG config - will only ask for password once per shell
keychain --eval --quiet >/dev/null

#
# Zsh specific goodies
#

# Prepare version control prompt
setopt prompt_subst # use substitutions in prompts
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

parse_git_stash () {
	[[ $(git stash list 2> /dev/null | tail -n1) != "" ]] && echo -e '\U1F95E '
}

# Simple vcs prompt
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}[%F{2}%b%F{5}]%f '
precmd () { vcs_info }
PROMPT='%n@%m %F{3}%c ${vcs_info_msg_0_}$(parse_git_stash)%f%% '

# OPTIONS
setopt auto_cd # change directory by typing a directory name on its own.
setopt extended_glob # Turn on the more powerful pattern matching features.
setopt histverify # Turn on verbose history substitution
setopt append_history # Prevents race conditions saving to history file
setopt hist_ignore_dups # Ignore duplicates in command history
setopt noclobber # Prevents redirected output from overwriting existing files
setopt nobeep
setopt nohashdirs # automatically find new executables

# Use 1000 history lines internally, save all of them to the file ~/.history
HISTSIZE=1000
SAVEHIST=$HISTSIZE
HISTFILE="$HOME/.history"
setopt HIST_IGNORE_SPACE

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

# AUTO COMPLETE
autoload -U compinit # Load the function-based completion system
compinit -u

# Load pager for long list of completion options
# Return advances one line, tab advances one page
zmodload zsh/complist
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' rehash true # look for new commands in PATH

# Load approximate completion for autocorrection
zstyle ':completion:::::' completer _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )'
zstyle ':completion:*corrections' format '%B%d (errors: %e)%b'

# Substitute xargs
autoload zargs

#
# Zsh Line Editor
#
# ZLE replaces readline; Fix it up to match readline
# Create a zkbd hash. To add other keys to this hash see man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"   beginning-of-buffer-or-history
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}" end-of-buffer-or-history

# Finally, make sure the terminal is in application mode when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
	function zle-line-init () {
		printf '%s' "${terminfo[smkx]}"
	}
	function zle-line-finish () {
		printf '%s' "${terminfo[rmkx]}"
	}
	zle -N zle-line-init
	zle -N zle-line-finish
fi
