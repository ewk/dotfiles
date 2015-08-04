#!/bin/bash

CWD="$(pwd)"

# Link home directory to config files in /dotfiles
ln -s "$CWD"/.ackrc "$HOME"
ln -s "$CWD"/.emacs.d "$HOME"
ln -s "$CWD"/.gitconfig "$HOME"
ln -s "$CWD"/.gitignore "$HOME"
ln -s "$CWD"/.gvimrc "$HOME"
ln -s "$CWD"/.rsyncignore "$HOME"
ln -s "$CWD"/.vim "$HOME"
ln -s "$CWD"/.vimrc "$HOME"
ln -s "$CWD"/.zprofile "$HOME"
ln -s "$CWD"/.zshenv "$HOME"
ln -s "$CWD"/.zshrc "$HOME"
ln -s "$CWD"/.mutt "$HOME"
ln -s "$CWD"/.notmuch-config "$HOME"

# Copy config and then enter password
cp .muttrc "$HOME"
cp .offlineimaprc "$HOME"

touch "$HOME"/.mutt/mutt-headercache

# Initialize Vim plugins stored in submodules
git submodule update --init --recursive
