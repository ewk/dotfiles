#!/bin/bash

cwd="$(pwd)"

# Link home directory to config files in /dotfiles
rm "$HOME"/.profile || true

ln -s "$cwd"/.gitconfig "$HOME"
ln -s "$cwd"/.gitignore "$HOME"
ln -s "$cwd"/.mutt "$HOME"
ln -s "$cwd"/.notmuch-config "$HOME"
ln -s "$cwd"/.zprofile "$HOME"/.zprofile
ln -s "$cwd"/.rsyncignore "$HOME"
ln -s "$cwd"/.tmux.conf "$HOME"
ln -s "$cwd"/.zlogout "$HOME"
ln -s "$cwd"/.zshenv "$HOME"
ln -s "$cwd"/.zshrc "$HOME"
ln -s "$cwd"/nvim "$HOME"/.config
ln -s "$cwd"/fish "$HOME"/.config
ln -s "$cwd"/alacritty "$HOME"/.config
ln -s "$cwd"/backup.sh "$HOME/bin/"


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
