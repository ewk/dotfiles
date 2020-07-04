#~/.zprofile
# This file is generally used for automatic execution of user scripts.
# It will be sourced when starting a login shell.

# Arch expects to find .zprofile. Create a symbolic link:
# ln -s dotfiles/.profile ~/.zprofile

# The zsh package in Debian ignores ~/.zprofile and expects to load the
# PATH from ~/.profile
# If ~/.bash_profile does not exist, ~/.bash_login and ~/.profile are checked
# in that order.

# Debian sets the default PATH for standard users from /etc/profile:
# /usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games
# Fix it:
#if [[ $(uname -a) == *'Debian'* ]]; then
	PATH="/sbin:/usr/sbin:$PATH"
#fi

# Set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

PATH="$HOME/.cargo/bin:$PATH"

export PATH
