PATH=$PATH:$HOME/bin:usr/local:/usr/local/lib:/usr/bin:
export PATH

#bindkey -me

# Load vcs prompt
# Brian Carper's git status line trick
autoload -Uz vcs_info
 
zstyle ':vcs_info:*' stagedstr '%F{28}●'
zstyle ':vcs_info:*' unstagedstr '%F{11}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git svn
precmd () {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{blue}]'
    } else {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{blue}]'
    }
 
    vcs_info
}
 
setopt prompt_subst
PROMPT='%F{blue}%n@%m %c${vcs_info_msg_0_}%F{blue} %(?/%F{blue}/%F{red})%% %{$reset_color%}'
# Old VCS
#autoload -Uz vcs_info
#precmd() {
#  psvar=()
#  vcs_info
#  [[ -n $vcs_info_msg_0_ ]] && psvar[1]="$vcs_info_msg_0_"
#}
  # set prompt to user@host current directory %  
#PS1='%n@%m%(1v.%F{green}%1v%f.) %. %# '
# END VCS 

# setopt prompt_subst # use substitutions in prompts
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
