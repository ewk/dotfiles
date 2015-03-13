#!/bin/bash

# make executable
# copy this line into /etc/anacrontab
# 1	5	backup.daily	su -c /home/ewk/Documents/dotfiles/backup.sh ewk

SOURCE=$HOME
DEST=/run/media/ewk/Backup
EXCLUDE=$HOME/.rsyncignore

date="$(date "+%Y-%m-%d-%H:%M:%S")"
# -a files are archived, characteristics are preserved
# -z compress data during transfer
# -v verbose
# -n dry run
# -P show progress
# -r recurse into directories; implied by -a

# include/exclude patterns are relative to $HOME
# remove the -$date suffix to use incremental backups
rsync -aPv --exclude-from="$EXCLUDE" "$SOURCE" "$DEST"/backup-"$date" > /dev/null &

