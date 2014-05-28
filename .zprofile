# resource limits for the shell
ulimit -S -n 1024

# GPG config
# keychain starts the gpg daemon for us
eval $(keychain --eval)
GPG_TTY=$(tty)
export GPG_TTY
