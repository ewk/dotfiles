#!/bin/bash

# Make this file executable
# Copy this line into /etc/anacrontab
# 1	5	backup.daily	su -c /home/ewk/Documents/dotfiles/backup.sh ewk
# May need to install and enable cronie
# Test with anacron -f
# Verify the timestamp file with 'cat /var/spool/anacron/backup.daily'

source=$HOME
dest=/run/media/ewk/Backup
exclude=$HOME/.rsyncignore

date="$(date "+%Y-%m-%d-%H:%M:%S")"

# -a files are archived, characteristics are preserved
# -z compress data during transfer
# -v verbose
# -n dry run
# -P show progress
# -r recurse into directories; implied by -a

# include/exclude patterns are relative to $HOME
# remove the -$date suffix to use incremental backups
if [ -d "$dest" ]; then
	rsync -aPv --exclude-from="$exclude" "$source" "$dest"/backup-"$date" > /dev/null &
	rsync -Pvr --exclude-from="$exclude" /etc "$dest" > /dev/null &
fi
