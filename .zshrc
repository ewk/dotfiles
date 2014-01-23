PATH=$PATH:$HOME/bin:usr/local:/usr/local/lib:/usr/local/mysql/bin:/usr/bin:
export PATH

# Use Emacs key bindings, C-e, C-a, etc
bindkey -e

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line

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

alias ll='ls -l'
alias pu=pushd
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
fi

# Because Emacs already has this
autoload -U tetris
zle -N tetris
bindkey KEYS tetris

# Sets the Mail Environment Variable
MAIL=/var/spool/mail/ewk && export MAIL

source ~/perl5/perlbrew/etc/bashrc
