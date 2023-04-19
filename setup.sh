#!/bin/bash

# Link home directory to config files in /dotfiles
ln -sf .gitconfig "$HOME"
ln -sf .gitignore "$HOME"
ln -sf .mutt "$HOME"
ln -sf .notmuch-config "$HOME"
ln -sf .rsyncignore "$HOME"
ln -sf .tmux.conf "$HOME"
ln -sf alacritty "$HOME"/.config
ln -sf backup.sh "$HOME/bin/"
ln -sf foot "$HOME"/.config
ln -sf fish "$HOME"/.config
ln -sf nvim "$HOME"/.config

# Set up systemd timer for mbsync
mkdir -p "$HOME"/.config/systemd/user
ln -sf mbsync.service "$HOME"/.config/systemd/user
ln -sf mbsync.timer "$HOME"/.config/systemd/user
sudo systemctl daemon-reload
systemctl --user start mbsync.timer
systemctl --user enable mbsync.timer
systemctl --user status mbsync.timer
