#!/bin/bash

cwd="$(pwd)"

# Link home directory to config files in /dotfiles
ln -s "$cwd"/.ackrc "$HOME"
ln -s "$cwd"/.emacs.d "$HOME"
ln -s "$cwd"/.gitconfig "$HOME"
ln -s "$cwd"/.gitignore "$HOME"
ln -s "$cwd"/.rsyncignore "$HOME"
ln -s "$cwd"/.vim "$HOME"
ln -s "$cwd"/.vim "$HOME"/.config/nvim
ln -s "$cwd"/.vimrc "$HOME"
rm "$HOME"/profile
rm "$HOME"/.bash_profile
ln -s "$cwd"/.profile "$HOME"
ln -s "$cwd"/.profile "$HOME"/.zprofile
ln -s "$cwd"/.profile "$HOME"/.bash_profile
ln -s "$cwd"/.zshenv "$HOME"
ln -s "$cwd"/.zshrc "$HOME"
ln -s "$cwd"/.zlogout "$HOME"
ln -s "$cwd"/.mutt "$HOME"
ln -s "$cwd"/.notmuch-config "$HOME"
ln -s "$cwd"/.tmux.conf "$HOME"
ln -s "$cwd"/.bashrc "$HOME"
ln -s "$cwd"/.bash_logout "$HOME"

# Copy config and then enter password
cp .mbsyncrc "$HOME"
cp .msmtprc "$HOME"

touch "$HOME"/.mutt/mutt-headercache
mkdir ~/.cache/zsh
touch ~/.cache/zsh/dirs
