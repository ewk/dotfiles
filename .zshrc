PATH=$PATH:$HOME/bin:usr/local:/usr/local/lib:/usr/bin:
export PATH

#bindkey -me
setopt prompt_subst # use substitutions in prompts
precmd() {
    psvar=()
    vcs_info
    [[ -n $vcs_info_msg_0_ ]] && psvar[1]="$vcs_info_msg_0_"
}

# You can now use `%1v' to drop the $vcs_info_msg_0_ contents in your prompt;

PS1="%n@%m %(1v.%F{red}%1v%f.)%# "

# Load vcs prompt
#autoload -Uz vcs_info
#zstyle ':vcs_info:*' enable git svn hg
#zstyle ':vcs_info:git*' formats "%s  %r/%S %b %m%u%c "
#precmd() {
#    vcs_info
#}
# END VCS 
# set prompt to user@host current directory %  
#PROMPT="%n@%m %~ ${vcs_info_msg_0_} %# "

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
alias 'open'='xdg-open'
