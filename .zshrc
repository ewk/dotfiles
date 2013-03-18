PATH=$PATH:$HOME/bin:usr/local:/usr/local/lib:/usr/bin:
export PATH

#bindkey -me

setopt prompt_subst # use substitutions in prompts
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}[%F{2}%b%F{5}]%f '
precmd () { vcs_info }

PS1='%n@%m %F{3}%c ${vcs_info_msg_0_}%f%# '

setopt auto_cd # change directory by typing a directory name on its own.
setopt extended_glob # Turn on the more powerful pattern matching features. 
setopt histverify # Turn on verbose history substitution 
setopt append_history # Prevents race conditions saving to history file
setopt hist_ignore_dups # Ignore duplicates in command history 
setopt noclobber # Prevents redirected output from overwriting existing files 

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
