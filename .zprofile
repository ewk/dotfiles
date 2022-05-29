# This file is generally used for automatic execution of user scripts.
# It will be sourced when starting a login shell.

typeset -U path PATH

# Include user's private bin if it exists.
if [ -d "$HOME/bin" ]; then
	path=($HOME/bin $path)
fi

path=($HOME/.cargo/bin $path)

export PATH
