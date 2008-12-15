#!/bin/bash

# Wrapper script around rsnapshot to identify my external usb-drive.
function get_backup_drive() 
{
    for f in /media/*; do
	if [ -d $f ] && [ -f $f/2ebc3fd6-7ffa-11dd-bc92-0017a4e4c205 ]; then
	    echo $f
	    return 0
	fi
    done

    echo "No backup drive found." >&2
    exit 1
}

conf_tmpl=$(cd $(dirname "$0"); pwd)/rsnapshot.conf.tmpl
conf=`pwd`/rsnapshot.conf
budrive=`get_backup_drive`
[ -z "$budrive" ] && exit 1

perl -pe "s@BACKUP_MEDIA@$budrive@g" $conf_tmpl > $conf

echo Backup drive: $budrive
echo Executing rsnapshot: "$@"
exec /usr/bin/rsnapshot -c $conf "$@"

