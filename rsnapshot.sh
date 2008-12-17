#!/bin/bash

# Wrapper script around rsnapshot to identify my external usb-drive.
# It checks all mounted drives under /media/ and searches for a
# special UUID-named file.
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

cp $cont_tmpl $conf
perl -i -pe "s@BACKUP_MEDIA@$budrive@g" $conf
perl -i -pe "s@HOSTNAME@`uname -n`@g" $conf

echo Backup drive: $budrive
echo Executing rsnapshot: "$@"
exec /usr/bin/rsnapshot -c $conf "$@"

