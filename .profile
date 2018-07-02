#~/.zprofile
# This file is generally used for automatic execution of user scripts.
# It will be sourced when starting a login shell.

# Arch expects to find .zprofile. Create a symbolic link:
# ln -s dotfiles/.profile ~/.zprofile

# The zsh package in Debian ignores ~/.zprofile and expects to load the
# PATH from ~/.profile

# Arch sets the default PATH from /etc/profile:
# PATH="/usr/local/sbin:/usr/local/bin:/usr/bin"
# /sbin is aliased to /bin on Arch systems
# Arch will also source additional paths (such as Perl) from /etc/profiles.d

# Debian sets the default PATH for standard users from /etc/profile:
# /usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games
# Fix it:
if [[ $(uname -a) == *'Debian'* ]]; then
	PATH=$PATH:/sbin:/usr/sbin
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

# Go expects one directory for all source files
export GOPATH=$HOME/Projects/GoWorkspace
PATH=$GOPATH/bin:$PATH

PATH="$HOME/.cargo/bin:$PATH"

export PATH
