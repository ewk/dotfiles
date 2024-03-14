# Used for setting user's interactive shell configuration and executing
# commands. Will be read when starting as an interactive shell.

#
# Completion
zmodload zsh/complist
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
zstyle ':completion:*:default' list-prompt '%S%M matches%s'	# Scroll long matches
zstyle ':completion:*' completer _complete _match _approximate	# Order of completers to try
zstyle ':completion:*:match:*' original only			# No wildcard matching

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

#
# Prompt
autoload -Uz promptinit
promptinit

# Prepare version control prompt
setopt prompt_subst
setopt transient_rprompt
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

parse_git_stash () {
       [[ $(git stash list 2> /dev/null | tail -n1) != "" ]] && echo -e '\U1F95E '
}

# vcs prompt
zstyle ':vcs_info:*' actionformats '%F{magenta}[%F{green}%b%F{yellow}|%F{red}%a%F{magenta}]%f '
zstyle ':vcs_info:*' formats '%F{magenta}[%F{green}%b%F{magenta}]%f '
precmd () { vcs_info }
PROMPT='%n@%m %F{yellow}%c ${vcs_info_msg_0_}$(parse_git_stash)%f%% '
RPROMPT='%(?..[%B%F{red}%?%f%b])'

#
# Environment, aliases, and functions
bindkey -e		# Use Emacs key bindings in terminal, C-e, C-a, etc

export EDITOR="nvim"
export SUDO_EDITOR="nvim"
export CSCOPE_EDITOR="nvim"
export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

alias dh='dirs -v'
alias gpg=gpg2
alias grep='egrep --color --ignore-case'
alias ll='ls --all --human-readable --group-directories-first -l'
alias ls='ls --color=auto'

open() {
	command xdg-open $1
}

# Colored man pages
man() {
	LESS_TERMCAP_mb=$'\e[1;31m' \
	LESS_TERMCAP_md=$'\e[1;38;5;74m' \
	LESS_TERMCAP_me=$'\e[0m' \
	LESS_TERMCAP_se=$'\e[0m' \
	LESS_TERMCAP_so=$'\e[38;5;246m' \
	LESS_TERMCAP_ue=$'\e[0m' \
	LESS_TERMCAP_us=$'\e[4;38;5;146m' \
	GROFF_NO_SGR=yes \
	command man "$@"
}

#
# Options
setopt auto_cd          # Change directory by typing a directory name on its own
setopt correct          # Automatically correct command names
setopt no_hup           # Don't kill background jobs when exiting
setopt no_beep          # I said no beeping
setopt no_clobber       # Prevents redirected output from overwriting existing files
setopt clobber_empty    # ... but do allow overwriting empty files
setopt no_hash_dirs     # When a command is hashed do not hash the directory containing it

#
# History
HISTSIZE=1000
SAVEHIST=$HISTSIZE
HISTFILE="$HOME"/.history
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt histverify       # Turn on verbose history substitution
setopt share_history    # Prevents race conditions saving to history file

# Map arrow keys to history
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#
# Directory stacks
DIRSTACKSIZE='20'

setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus
setopt pushd_silent
setopt pushd_to_home

#
# ZLE
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi
