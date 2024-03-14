# Used for setting user's environment variables; it should not contain commands
# that produce output or assume the shell is attached to a TTY. When this file
# exists it will always be read.

# Do not add duplicates to $PATH
typeset -U path PATH

# Include user's private bin if it exists.
if [ -d "$HOME/bin" ]; then
	path=($HOME/bin $path)
fi

path=($HOME/.cargo/bin $path)

export PATH
