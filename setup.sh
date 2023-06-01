#!/bin/bash

cwd="$(pwd)"

# Link home directory to config files in /dotfiles
ln -sf "$cwd"/.gitconfig "$HOME"
ln -sf "$cwd"/.gitignore "$HOME"
ln -sf "$cwd"/.mutt "$HOME"
ln -sf "$cwd"/.notmuch-config "$HOME"
ln -sf "$cwd"/.rsyncignore "$HOME"
ln -sf "$cwd"/alacritty "$HOME"/.config
ln -sf "$cwd"/backup.sh "$HOME/bin/"
ln -sf "$cwd"/fish "$HOME"/.config
ln -sf "$cwd"/nvim "$HOME"/.config

# Set up systemd timer for mbsync
mkdir -p "$HOME"/.config/systemd/user
ln -sf "$cwd"/mbsync.service "$HOME"/.config/systemd/user
ln -sf "$cwd"/mbsync.timer "$HOME"/.config/systemd/user
sudo systemctl daemon-reload
systemctl --user start mbsync.timer
systemctl --user enable mbsync.timer
systemctl --user status mbsync.timer
