#!/bin/bash

cwd="$(pwd)"

# Link home directory to config files in /dotfiles
ln -sf "$cwd"/.gitconfig "$HOME"
ln -sf "$cwd"/.gitignore "$HOME"
ln -sf "$cwd"/.mutt "$HOME"
ln -sf "$cwd"/.notmuch-config "$HOME"
ln -sf "$cwd"/.rsyncignore "$HOME"
ln -sf "$cwd"/.tmux.conf "$HOME"
ln -sf "$cwd"/.zlogout "$HOME"
ln -sf "$cwd"/.zprofile "$HOME"/.zprofile
ln -sf "$cwd"/.zshenv "$HOME"
ln -sf "$cwd"/.zshrc "$HOME"
ln -sf "$cwd"/alacritty "$HOME"/.config
ln -sf "$cwd"/backup.sh "$HOME/bin/"
ln -sf "$cwd"/fish "$HOME"/.config
ln -sf "$cwd"/nvim "$HOME"/.config

touch "$HOME"/.mutt/mutt-headercache

# Set up systemd timer for mbsync
mkdir -p "$HOME"/.config/systemd/user
ln -s "$cwd"/mbsync.service  "$HOME"/.config/systemd/user
ln -s "$cwd"/mbsync.timer "$HOME"/.config/systemd/user

echo
echo 'Reload the systemd daemon to load mbsync.timer'
sudo systemctl daemon-reload
systemctl --user start mbsync.timer
systemctl --user enable mbsync.timer
