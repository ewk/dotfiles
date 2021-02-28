#~/.zprofile
# This file is generally used for automatic execution of user scripts.
# It will be sourced when starting a login shell.

# Set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin:$PATH"
fi

PATH="$HOME/.cargo/bin:$PATH"

export PATH
