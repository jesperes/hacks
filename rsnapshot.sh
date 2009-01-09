#!/bin/bash

RSNAPSHOT=/usr/bin/rsnapshot

# Wrapper script around rsnapshot to identify my external usb-drive.
# It checks all mounted drives under /media/ and searches for a
# special UUID-named file.
function get_backup_drive()
{
    for f in /media/*/2ebc3fd6-7ffa-11dd-bc92-0017a4e4c205; do
	dirname "$f"
	return 0
    done

    echo "Backup drive not found." >&2
    exit 1
}

home=$(cd $(dirname "$0"); pwd)
conf_tmpl=$home/rsnapshot.conf.tmpl
conf=$home/rsnapshot.conf
budrive=`get_backup_drive`/backups
[ -z "$budrive" ] && exit 1

cp $conf_tmpl $conf
perl -i -pe "s@BACKUP_MEDIA@$budrive@g" $conf
perl -i -pe "s@HOSTNAME@`uname -n`@g" $conf

action=$1
if [ $action = sync ]; then
    echo Backup to: $budrive
fi
$RSNAPSHOT -c $conf $action
