#!/bin/bash

set -e
: ${MPLAYER=mplayer.exe}
: ${MENCODER=mencoder.exe}

exec 2>&1

if [ -z "$PROFILE" ]; then
    read -p "Profile: " PROFILE
else
    echo "Profile: $PROFILE"
fi

INPUTFILE="$1"; shift
OUTPUTFILE="$1"; shift

if [ -f "$OUTPUTFILE" ]; then
    echo "Output file already exists."
    exit 1
fi

if [ x$DVDDEVICE != x ]; then
    DVDDEV="-dvd-device $DVDDEVICE"
fi

MENC_OPTS="-priority idle -ffourcc XVID -idx"
SCALE=scale
PP=pp=default
ASPECT=autoaspect
PASSES=2

case $PROFILE in
    # For doing DVD rips
    dvd)
	# Video bitrate
	BITRATE=1600
	
	# Audio bitrate when re-encoding to mp3. Audio copied if empty.
	ABITRATE=

	# Cropping filter. Autodetect if empty.
	CROP=
	;;

    *)
	echo "No profile set."
	exit 1
	;;
esac

function crop_detect() {
    INPUTFILE="$1"; shift
    TMP=cropdetect$$.log
    rm -f $TMP

    set -x
    
    $MPLAYER "$INPUTFILE" -vf cropdetect,scale -vo directx -ao null \
	-sstep 120 -frames 20 $DVDDEV >$TMP 2>&1

    CROP=$(grep "CROP" $TMP | tail -n 1 | sed -e 's/.*-vf \(crop.*\))\./\1/')
    if [ x$CROP = x ]; then
	echo "*** Crop detection failed. Running again to display any errors: " >&2
	$MPLAYER "$INPUTFILE" -vf cropdetect,scale -vo directx -ao null \
	    -sstep 120 -frames 20 $DVDDEV >&2
	echo "crop"
    else
	echo $CROP
    fi

    rm -f $TMP
}

if [ x$CROP = x ]; then
    echo
    echo "Checking cropping... "
    echo
    CROP=$(crop_detect "$INPUTFILE")
fi

if [ x$BITRATE = x ]; then
    OVC="-ovc copy"
    BITRATE=800
else
    OVC="-ovc lavc"
fi

if [ x$ABITRATE = x ] ; then
    OAC="-oac copy"
    LAVCOPTS="-lavcopts vcodec=mpeg4:vbitrate=$BITRATE:$ASPECT"
else
    OAC="-oac lavc"
    LAVCOPTS="-lavcopts vcodec=mpeg4:vbitrate=$BITRATE:acodec=mp3:abitrate=$ABITRATE:$ASPECT"
fi

export TIMEFORMAT="Elapsed time: %lR"

export MENCODER MENC_OPTS OAC OVC CROP SCALE PP DVDDEV

function run_mencoder() {
    echo "---------------------------"
    echo "Audio coding: $OAC"
    echo "Video coding: $OVC"
    echo "LAVCOPTS=$LAVCOPTS"
    echo "CROP=$CROP"
    echo "PP=$PP"
    echo "SCALE=$SCALE"
    echo "---------------------------"

    if [ x$PREVIEW != x ]; then
	PREVIEW="-frames 1000"
	echo "*** PREVIEW ONLY ***"
    fi
    
    set -x
    time $MENCODER $MENC_OPTS "$INPUTFILE" -o "$OUTPUTFILE" \
	$OAC $OVC "$@" -vf-pre $CROP,$PP,$SCALE,harddup $PREVIEW $DVDDEV
    set +o xtrace
}

if [ $PASSES = 1 ]; then
    LAVCOPTS="$LAVCOPTS:v4mv:trell:mbd=2"

    echo "Encoding: $INPUTFILE"
    run_mencoder $LAVCOPTS
else
    LAVCOPTS1="$LAVCOPTS:turbo:vpass=1"
    LAVCOPTS2="$LAVCOPTS:mbd=2:trell:v4mv:vpass=2"

    echo "Encoding (pass 1): $INPUTFILE"
    run_mencoder $LAVCOPTS1

    echo "Encoding (pass 2): $INPUTFILE"
    run_mencoder $LAVCOPTS2
fi

rm -f divx2pass.log
