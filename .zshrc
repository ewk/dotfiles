# .zshrc main configuration file

# Version control
setopt prompt_subst # use substitutions in prompts
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
# git status line traffic light trick, more of a pain than it's worth
#zstyle ':vcs_info:*' stagedstr '%F{28}●'
#zstyle ':vcs_info:*' unstagedstr '%F{11}●'
#zstyle ':vcs_info:*' check-for-changes true
#precmd () {
#    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
#        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}]'
#    } else {
#        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{blue}]'
#    }
#  }
#PROMPT='%n@%m %F{3}%c${vcs_info_msg_0_}%F{blue} %(?/%F{blue}/%F{red})%{$reset_color%}%f%# '

#simple vcs
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}[%F{2}%b%F{5}]%f '
precmd () { vcs_info }
PS1='%n@%m %F{3}%c ${vcs_info_msg_0_}%f%# '
# End simple vcs

# resource limits for the shell
ulimit -S -n 1024

setopt auto_cd # change directory by typing a directory name on its own.
setopt extended_glob # Turn on the more powerful pattern matching features. 
setopt histverify # Turn on verbose history substitution 
setopt append_history # Prevents race conditions saving to history file
setopt hist_ignore_dups # Ignore duplicates in command history 
setopt noclobber # Prevents redirected output from overwriting existing files 
setopt nobeep

# Use 1000 history lines internally, save all of them to the file ~/.history 
HISTSIZE=100
SAVEHIST=100
HISTFILE=~/.bash_history

alias pu=pushd
alias ll='ls -l'
# alias rm='rm -i'
# Load the function-based completion system 
autoload -U compinit
compinit

# Load pager for long list of completion options
# Return advances one line, tab advances one page
zmodload zsh/complist 
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Load approximate completion for autocorrection
zstyle ':completion:::::' completer _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )' 
zstyle ':completion:*corrections' format '%B%d (errors: %e)%b'

# User specific aliases and functions
alias sudo='sudo env PATH=$PATH'
if [[ `uname` == 'Linux' ]]
then
  alias 'open'='xdg-open'
fi

if [[ `uname` == 'Darwin' ]]
then
    alias 'gitk'='gitx'
else
    alias 'gitk'='(gitk&)'
fi

# Because Emacs already has this
autoload -U tetris
zle -N tetris
bindkey KEYS tetris

# Sets the Mail Environment Variable
MAIL=/var/spool/mail/ewk && export MAIL

# perlbrew config
export PERLBREW_ROOT=~/bin/perl5
source ~/bin/perl5/etc/bashrc

# fix up zle; replaces readline
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
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

# Finally, make sure the terminal is in application mode, when zle is
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

# dirs -v Remember last visited folders
# cd - TAB autcomplete
DIRSTACKSIZE=20
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >>$DIRSTACKFILE
}

setopt autopushd pushdsilent pushdtohome

# Remove duplicate entries
#setopt pushdignoredups
typeset -U dirstack

# This reverts the +/- operators.
setopt pushdminus

# Ack
ACKRC=~/.ackrc
#ACK_OPTIONS

# Grep
alias grep='egrep --color'
GREP_COLOR='1;30;43'

