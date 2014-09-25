# set the command search path and environment variables;
# this file should not contain commands that produce output
for d in /$HOME/bin/*/; do
    PATH+=":$d"
done
PATH=/usr/local/bin:/usr/local/sbin:/usr/bin/vendor_perl:/usr/bin:/sbin:$PATH:
export PATH

export EDITOR="vim"
# But use Emacs key bindings in terminal, C-e, C-a, etc
bindkey -e

# GPG config
# keychain starts the gpg daemon for us
eval $(keychain --eval --quiet)
GPG_TTY=$(tty)
export GPG_TTY

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line

# Colors and rainbows
export TERM=xterm-256color
export CLICOLOR=1 # Enable colorized output; equivalent to ls -G on Darwin

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
