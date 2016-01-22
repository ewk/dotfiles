#!/bin/bash

# make executable
# copy this line into /etc/anacrontab
# 1	5	backup.daily	su -c /home/ewk/Documents/dotfiles/backup.sh ewk
# May need to install and enbale cronie
# Verify 'cat /var/spool/anacron/backup.daily'

source=$HOME
postfix=/etc/postfix
logwatch=/etc/logwatch
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
rsync -aPv --exclude-from="$exclude" "$source" "$dest"/backup-"$date" > /dev/null &
# Skips /etc/postfix/sasl_passwd
rsync -Pv --exclude-from="$exclude" "$postfix" "$dest" > /dev/null &
rsync -Pv --exclude-from="$exclude" "$logwatch" "$dest" > /dev/null &

