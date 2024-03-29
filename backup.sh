#!/bin/bash

# Make this file executable
# Copy this line into /etc/anacrontab
# 1	5	backup.daily	su -c /home/ewk/Documents/dotfiles/backup.sh ewk
# May need to install and enable cronie
# Test with anacron -f
# Verify the timestamp file with 'cat /var/spool/anacron/backup.daily'

source=$HOME
dest=/run/media/"$USER"/Backup
exclude=$HOME/.rsyncignore
host="$(hostname)"

date="$(date "+%Y-%m-%d-%H:%M:%S")"

# -a files are archived, characteristics are preserved
# -z compress data during transfer
# -v verbose
# -n dry run
# -P show progress
# -r recurse into directories; implied by -a

# include/exclude patterns are relative to $HOME
# remove the -$date suffix to use incremental backups
if [ -e "$dest" ]; then
	# Delete backup folders older than 6 months
	echo
	echo "Deleting old backups ..."
	echo
	cd "$dest"
	find "$dest"/"$host"/* -maxdepth 0 -type d -ctime +180 -exec rm -rf {} \;

	rsync -aPv --exclude-from="$exclude" "$source" "$dest"/"$host"/backup-"$date" > /dev/null &
	rsync -aPv "$HOME"/Music "$dest" > /dev/null &
else
	echo "Backup drive not available."
fi
