#!/bin/bash

cwd="$(pwd)"

# Link home directory to config files in /dotfiles
ln -s "$cwd"/.emacs.d "$HOME"
ln -s "$cwd"/.gitconfig "$HOME"
ln -s "$cwd"/.gitignore "$HOME"
ln -s "$cwd"/.rsyncignore "$HOME"
ln -s "$cwd"/.vim "$HOME"
ln -s "$cwd"/.vim "$HOME"/.config/nvim
ln -s "$cwd"/.vimrc "$HOME"
rm "$HOME"/profile
ln -s "$cwd"/.profile "$HOME"
ln -s "$cwd"/.profile "$HOME"/.zprofile
ln -s "$cwd"/.zshenv "$HOME"
ln -s "$cwd"/.zshrc "$HOME"
ln -s "$cwd"/.zlogout "$HOME"
ln -s "$cwd"/.mutt "$HOME"
ln -s "$cwd"/.notmuch-config "$HOME"
ln -s "$cwd"/.tmux.conf "$HOME"

# Copy config and then enter password
cp -n .mbsyncrc "$HOME"
cp -n .msmtprc "$HOME"

touch "$HOME"/.mutt/mutt-headercache

# Set up systemd timer for mbsync
mkdir -p "$HOME"/.config/systemd/user
ln -s "cwd"/mbsync.service  "$HOME"/.config/systemd/user
ln -s "cwd"/mbsync.timer "$HOME"/.config/systemd/user

echo
echo 'Reload the systemd daemon to load mbsync.timer'
echo '	$ systemctl daemon-reload'
echo
echo 'Then enable the timer:'
echo '	$ systemctl --user start mbsync.timer'
echo '	$ systemctl --user enable mbsync.timer'
