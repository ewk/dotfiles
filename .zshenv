# Set the command search path and environment variables.
# This file is sourced on all invocations of the shell.
# This file should not contain commands that produce output.

# PATH
# /etc/zprofile usually does this for all users' home directories
# The PATH+= syntax is brittle. Better to avoid it.
for d in $HOME/bin/*; do
    PATH="$d":$PATH
done
# Path to Go installation, since package manager is out of date
export GOROOT=$HOME/bin/gosource
# Go expects one directory for all source files
export GOPATH=$HOME/Projects/GoWorkspace
PATH=$GOROOT/bin:$GOPATH/bin:$HOME/bin:$PATH
export PATH

# EDITOR
export EDITOR="gvim -f"
# But use Emacs key bindings in terminal, C-e, C-a, etc
bindkey -e

# Enable Ctrl-x-e to edit command line; same as typing 'fc"
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Colors and rainbows
export TERM=xterm-256color
export CLICOLOR=1 # Enable color output on OS X

# Colored man pages
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}
