#~/.zprofile
# This file is generally used for automatic execution of user's scripts.

# Normally, the path should be set in ~/.zshenv, but Arch Linux
# sources /etc/profile after sourcing ~/.zshenv.
# To prevent your $PATH being overwritten, set it in ~/.zprofile.

# PATH
# /etc/zprofile usually does this for all users' home directories
# The PATH+= syntax is brittle. Better to avoid it.
for d in $HOME/bin/*; do
    PATH="$d":$PATH
done

# Go expects one directory for all source files
export GOPATH=$HOME/Projects/GoWorkspace
PATH=$GOPATH/bin:$HOME/bin:$PATH
export PATH

