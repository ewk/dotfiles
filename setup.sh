#!/bin/bash

HOME=/home/ewk

# Link home directory to config files in /dotfiles
ln -s .ackrc $HOME
ln -s .emacs.d $HOME
ln -s .gitconfig $HOME
ln -s .gitignore $HOME
ln -s .gvimrc $HOME
ln -s .irbrc $HOME
ln -s .rsyncignore $HOME
ln -s .vim $HOME
ln -s .vimrc $HOME
ln -s .zprofile $HOME
ln -s .zshenv $HOME
ln -s .zshrc $HOME

# Initialize Vim plugins stored in submodules
git submodule update --init --recursive
