#!/bin/sh
rm -f /media/backup-media
for f in /media/*; do
    if [ -d $f ]; then
	if [ -f $f/2ebc3fd6-7ffa-11dd-bc92-0017a4e4c205 ]; then
	    ln -vs $f /media/backup-media
	fi
    fi
done

if [ -d /media/backup-media ]; then
    echo Executing rsnapshot "$@"
    exec rsnapshot "$@"
else
    echo "Failed to find backup media."
    exit 1
fi
